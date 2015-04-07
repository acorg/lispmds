/*
changes for lisp:
  pass the output file as a parameter
  stop when no total lms change for 1000 iterations
  write the clusters to a file (in lisp format)
  remove some of the debug output

remember in lisp that we need the same number of strains in the same order in both files

gcc -O -o Procrustes-Kmeans-for-lisp Procrustes-Kmeans-for-lisp.c
Procrustes-Kmeans-for-lisp inputfilename1 inputfilename2 num-dims num-clusters 0.1 seed outputfilename scalep>& diagnostics

TODO: increase the max-examples to 2000 (for all-strains), currently i get a seg fault if i set max_examples to 2000

TODO: for compiling on windows, link failure for random and srandom (i've had that before, and i don't remember the work around.

*/


#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* derek #define MAX_EXAMPLES 500 */
#define MAX_EXAMPLES 600    /* cannot get 2000, why not? */
#define MAX_DIM 10
#define BUFSIZE 1024
#define TRUE 1
#define FALSE 0

/* Made this global to get svd interfacing going quickly */
float a[MAX_DIM][MAX_DIM],v[MAX_DIM][MAX_DIM],w[MAX_DIM];

/*
   Made these global to save long argument passing to functions.
   Ought to be in a structure and pass pointer: whole code needs cleanup.
*/
int Cluster[MAX_EXAMPLES][MAX_EXAMPLES],ClusterLabel[MAX_EXAMPLES];
int NumInCluster[MAX_EXAMPLES],Dim,K;
float Data1[MAX_EXAMPLES][MAX_DIM],Data2[MAX_EXAMPLES][MAX_DIM];
char Data1ID[MAX_EXAMPLES][BUFSIZE],Data2ID[MAX_EXAMPLES][BUFSIZE];
int ScaleFlag;

main(argc,argv)
int argc;
char *argv[];
{

FILE *fn1, *fn2;
FILE *fnOut;  /* derek */

int N1,N2,NumMatched=0,ID,NumOnMatchList=0,l,label,cluster,c,c_min;
int test_point,old_test_point,new_test_point_cluster,old_test_point_cluster;
int Seed,member,NumTries,Done;
int index[MAX_EXAMPLES],Index[MAX_EXAMPLES][MAX_EXAMPLES];
register int d,i,j,k,m,n,cntr;
register int num1=0,num2=0,DumpLoops=1000;

float Procrustes();
float LMS[MAX_EXAMPLES],TotalLMS,TestTotalLMS;
float dist[MAX_EXAMPLES],Distance[MAX_EXAMPLES][MAX_EXAMPLES];
float Threshold,Min, LMS_old_test_point_cluster, LMS_new_test_point_cluster;
float MatchedData1[MAX_EXAMPLES][MAX_DIM],MatchedData2[MAX_EXAMPLES][MAX_DIM];

float previousDumpLoopCheckpointTotalLMS;  /* derek, we are going to stop when TotalLMS has not changed since last DumpLoop iterations checkpoint */


char MatchedData1ID[MAX_EXAMPLES][BUFSIZE];
char MatchedData2ID[MAX_EXAMPLES][BUFSIZE];
char LockFlag,Name1[BUFSIZE],Name2[BUFSIZE];



/* PURPOSE:
Finds common clusters in two files getting Procrusted: you set
the number of clusters, it finds that many clusters that best match
via Procrustes together between the two files.

The algorithm is basically a Kmeans style algorithm:
(1) Initialise by picking K cluster centers at random and assigning all points 
to nearest cluster center (could assign totally at random, but this
probably makes better initial clusters). A cluster center is a given datapoint.
(2) Then in a loop: randomly pick a point and randomly reassign it to a
different cluster, keeping the change if the TotalLMS error goes down,
rejecting the change if it goes up.
*/

   if(argc!=9) {    /* derek was 7, now we add the output filename */
		    /* asl: added ScaleFlag so now is changed from derek 8 to 9 */
      fprintf(stderr,"Use: %s DataFile1 DataFile2 Dimension Kclumps ThresholdClumps Seed OutputFile Scaling[1/0]\n", argv[0]);
      exit(1);
      }

   Dim=atoi(argv[3]);K=atoi(argv[4]); Threshold=atof(argv[5]);
   Seed=atoi(argv[6]); srandom(Seed); ScaleFlag=atoi(argv[8]);
   printf("DataFile1=%s DataFile2=%s Dimension=%d K=%d Threshold=%f Seed=%d ScaleFlag=%d\n",
           argv[1], argv[2],Dim,K,Threshold, Seed,ScaleFlag);

   for(cluster=0;cluster<K;cluster++) NumInCluster[cluster]=0;

   /* read in the matched data files */
   if((fn1=fopen(argv[1],"r"))==NULL) {
      fprintf(stderr,"Can't open %s\n",argv[1]);
      exit(1);
      }
   num1=ReadData(fn1,Data1,Data1ID); fclose(fn1);
   if((fn2=fopen(argv[2],"r"))==NULL) {
      fprintf(stderr,"Can't open %s\n",argv[2]);
      exit(1);
      }
   num2=ReadData(fn2,Data2,Data2ID); fclose(fn2);

   fprintf(stderr,"Read: num1= %d num2= %d examples\n",num1,num2);
   if(num1!=num2)
      {fprintf(stderr,"num1=%d not equal to num2=%d: not matched files!\n",
       num1,num2); exit(1); }
   else NumMatched=num1;

   /* derek output file */
   if((fnOut=fopen(argv[7],"w"))==NULL) {
      fprintf(stderr,"Can't open %s\n",argv[7]);
      exit(1);
      }


   /* get initial global LMS, i.e. with no clusters defined, by temporarily
      putting all points in Cluster i=0
   */
   i=0;
   NumInCluster[i]=NumMatched;
   for(j=0;j<NumInCluster[i];j++) Cluster[i][j]=j;

   GetDataInCluster(i,MatchedData1,MatchedData2);
   TotalLMS=Procrustes(NumInCluster[i],MatchedData1,MatchedData2);

   printf("Snapshot:  (global)  TotalLMS= %f AveTotalLMS= %f\n",TotalLMS,TotalLMS/NumMatched);
   for(i=0;i<NumMatched;i++) NumInCluster[i]=0;

   /*
   Pick initial clusters as those closest *in file 2* to the picked seed.
   Could equally have based on file 1. Could also have assigned random
   points to random initial clusters, but it's more likely that spatially
   nearby points will end up in a common cluster, so I've initialized this 
   way.
   */

   /* pick the cluster centers */
   for(i=0;i<K;i++) ClusterLabel[i]=random()%NumMatched;

   /* go through points, assign point to cluster with closest seed point */
   for(i=0;i<NumMatched;i++) {
      for(c=0;c<K;c++) {
	 label=ClusterLabel[c];
         for(k=0;k<Dim;k++) 
	 dist[c] += (Data2[label][k]-Data2[i][k])*
		    (Data2[label][k]-Data2[i][k]);
         dist[c]=sqrt(dist[c]);
	 }
      /* find cluster that's minimum distance to point i */
      Min=1e9;
      for(c=0;c<K;c++) if(dist[c]<Min) {Min=dist[c];c_min=c;};
      /* assign point i to that cluster */
      cntr=NumInCluster[c_min];
      Cluster[c_min][cntr]=i;
      NumInCluster[c_min]++;

       }

   /* compute the initial total least mean square error, LMS */
   TotalLMS=0.;
   for(i=0;i<K;i++) {
      if(NumInCluster[i]<=(Dim+1))
         { fprintf(stderr,"Cluster[%d] has only %d \n",
	   i,NumInCluster[i]);continue;
	 }
      GetDataInCluster(i,MatchedData1,MatchedData2);
      printf("\nDEBUG: Cluster=%d ClusterCenter=%d NumInCluster=%d\n",
                                   i,ClusterLabel[i],NumInCluster[i]);
      LMS[i]=Procrustes(NumInCluster[i],MatchedData1,MatchedData2);
      TotalLMS += LMS[i];
      }


   /* print some initial info */
   printf("\nDEBUG\n");
   for(i=0;i<K;i++)
   if(NumInCluster[i]>(Dim+1))  printf("Initial LMS[%d]= %f\n",i,LMS[i]);
   printf("TotalLMS=%f\n",TotalLMS);

   /* derek: set inital previous checkpoint to be higher than intial so we dont terminate at the start */
   previousDumpLoopCheckpointTotalLMS = TotalLMS + 1000.0;  

   /* here comes the major iterative loop */
   for(NumTries=0;NumTries<100000;NumTries++)
   {

      /* pick a test point to move and get its present cluster */
      test_point=random()%NumMatched;
      old_test_point_cluster=GetClusterOf(test_point);

      /* choose new_test_cluster not equal to present one */
      /* also don't move things to clusters with less than Dim+1 points */
      /* clusters with Dim+1 points can hang around, I need to introduce
	 some small cluster cleanup routine later
      */
      Done=FALSE;
      while(!Done) {
	 new_test_point_cluster=random()%K;
	 if(NumInCluster[new_test_point_cluster]<(Dim+1)||
	    new_test_point_cluster==old_test_point_cluster)
	    Done=FALSE;
	 else Done=TRUE;
	 }

      /* move and delete */
      Add(test_point,new_test_point_cluster);
      Delete(test_point,old_test_point_cluster);

      /* eval LMS again keeping copy of old values in case we reject the move */
      i=old_test_point_cluster;
      if(NumInCluster[i]>=(Dim+1)) {
	 LMS_old_test_point_cluster=LMS[i]; /*copy */
	 GetDataInCluster(i,MatchedData1,MatchedData2);
	 LMS[i]=Procrustes(NumInCluster[i],MatchedData1,MatchedData2);
	 }
      else
	 fprintf(stderr,"Cluster[%d] has %d \n",i,NumInCluster[i]);

      i=new_test_point_cluster;
      if(NumInCluster[i]>=(Dim+1)) {
	 LMS_new_test_point_cluster=LMS[i]; /*copy */
	 GetDataInCluster(i,MatchedData1,MatchedData2);
	 LMS[i]=Procrustes(NumInCluster[i],MatchedData1,MatchedData2);
	 }
      else
	 fprintf(stderr,"Cluster[%d] has %d \n",i,NumInCluster[i]);

      /* update the TestTotalLMS */
      TestTotalLMS= TotalLMS -
		    LMS_old_test_point_cluster -  LMS_new_test_point_cluster +
		    LMS[old_test_point_cluster] + LMS[new_test_point_cluster];


	 /* if LMS went up reverse this, else do nothing (i.e. keep it) */
	 if(TestTotalLMS>TotalLMS) {
	    Add(test_point,old_test_point_cluster);
	    Delete(test_point,new_test_point_cluster);
	    LMS[new_test_point_cluster]=LMS_new_test_point_cluster;
	    LMS[old_test_point_cluster]=LMS_old_test_point_cluster;
	    }
	 else if(TestTotalLMS<=TotalLMS)
	    {TotalLMS=TestTotalLMS;
	     printf("Accepting Move: TotalLMS= %f NumTries= %d\n",
		     TotalLMS,NumTries);fflush(stdout);
	    }


	  /* dump out info every fixed number of attempts */
	  if(NumTries%DumpLoops==0) {

	    printf("\nSnapshot: NumTries=%d TotalLMS= %f AveTotalLMS= %f\n",
		   NumTries,TotalLMS,TotalLMS/NumMatched);

	    for(i=0;i<K;i++)
	       printf("Snapshot: NumTries= %d: Cluster= %d NumInCluster= %d LMS[%d]= %f AveLMS[%d]= %f\n",NumTries,i,NumInCluster[i],
		       i,LMS[i],i,LMS[i]/NumInCluster[i]);

	    /* derek: not needed when we run for lisp 
	    for(i=0;i<K;i++) {

	       printf("\nSnapshot NumTries= %d: Cluster= %d NumInCluster= %d LMS[%d]= %f AveLMS[%d]= %f\n",NumTries,i,NumInCluster[i],
	           i,LMS[i],i,LMS[i]/NumInCluster[i]);

	       for(l=0;l<NumInCluster[i];l++)
		  {member=Cluster[i][l];
		   printf("NumTries= %d: Cluster= %d member= %d Data2ID[%d]= %s\n", NumTries,i,member,member,Data2ID[member]);
		   }

	       }
	    */
	    printf("\n");
	    fflush(stdout);
	    }

	  /* derek: terminate the loop if the TotalLMS did not change between DumpLoops iterations */
	  if(NumTries%DumpLoops==0) {
	      if (previousDumpLoopCheckpointTotalLMS==TotalLMS) {
		  /* write the clusters to the output file */
		  for(i=0;i<K;i++) {
		      fprintf(fnOut, "((cluster %d)", i);
		      for(l=0;l<NumInCluster[i];l++) 
			  fprintf(fnOut, " %d", Cluster[i][l]);
		      fprintf(fnOut, ")\n");
		  }
		  fprintf(fnOut, "(lms-error %f)\n", TotalLMS);
		  fflush(fnOut);

		  /* terminate the loop */
		  NumTries=100000;  
	      }
	      previousDumpLoopCheckpointTotalLMS=TotalLMS;
	  }

   }
      

}

Add(test_point,new_cluster)
int test_point,new_cluster;
{
      /* moves by adding to end of list */
      Cluster[new_cluster][NumInCluster[new_cluster]]=test_point;
      NumInCluster[new_cluster]++;
}

Delete(point,cluster)
int point,cluster;
{
int l,n,ClusterCopy[MAX_EXAMPLES][MAX_EXAMPLES],cntr=0;

   for(l=0;l<NumInCluster[cluster];l++) if(Cluster[cluster][l]==point) break;
   /* so want to delete the lth one */
   for(n=0;n<NumInCluster[cluster];n++) 
      if(n!=l) {ClusterCopy[cluster][cntr]=Cluster[cluster][n]; cntr++;}

   NumInCluster[cluster]=cntr;

   for(n=0;n<NumInCluster[cluster];n++) 
      Cluster[cluster][n]=ClusterCopy[cluster][n];
}

GetClusterOf(test_point)
int test_point;
{
int k,l,cluster;

   for(k=0;k<K;k++) 
      for(l=0;l<NumInCluster[k];l++) 
         if(Cluster[k][l]==test_point) {cluster=k;break;}

   return(cluster);
}

GetDataInCluster(i,MatchedData1,MatchedData2)
int i;
float MatchedData1[MAX_EXAMPLES][MAX_DIM],MatchedData2[MAX_EXAMPLES][MAX_DIM];
{
int l,label,d,member;

   for(l=0;l<NumInCluster[i];l++) {
       label=ClusterLabel[i]; member=Cluster[i][l];
       for(d=0;d<Dim;d++) { MatchedData1[l][d]=Data1[member][d];
			    MatchedData2[l][d]=Data2[member][d]; }
       }

}


float Procrustes(NumMatched,MatchedData1,MatchedData2)
float MatchedData1[MAX_EXAMPLES][MAX_DIM],MatchedData2[MAX_EXAMPLES][MAX_DIM];
int NumMatched;
{
float Transform2[MAX_EXAMPLES][MAX_DIM];
float Transform2Matched[MAX_EXAMPLES][MAX_DIM];
float Intermediate1[MAX_EXAMPLES][MAX_DIM],Intermediate2[MAX_EXAMPLES][MAX_DIM];
float Intermediate3[MAX_EXAMPLES][MAX_DIM],Intermediate4[MAX_EXAMPLES][MAX_DIM];
float Intermediate5[MAX_EXAMPLES][MAX_DIM],Intermediate6[MAX_EXAMPLES][MAX_DIM];
float Translation[MAX_DIM];
float J[MAX_EXAMPLES][MAX_EXAMPLES],Jsqr[MAX_EXAMPLES][MAX_EXAMPLES],OneOverN;
float T[MAX_DIM][MAX_DIM];
float Scale=1., LMS=0.,tmp;
float Denom[MAX_DIM][MAX_DIM],TraceDenominator=0.;
float Numerator[MAX_DIM][MAX_DIM],TraceNumerator=0.;
float AveDist,SD;
void svdcmp();
int i,j,k,m,n;

        
   if(ScaleFlag) {
   /* create J matrix */
   OneOverN=1./NumMatched;
   for(i=0;i<NumMatched;i++)
      for(j=0;j<NumMatched;j++){
	 if(i==j) J[i][j]=1;
	 else J[i][j]=0;
	 J[i][j] -=OneOverN;
	 }
     /* create Jsqr matrix */
     for(i=0;i<NumMatched;i++)
        for(j=0;j<NumMatched;j++) {
        Jsqr[i][j]=0.;
        for(k=0;k<NumMatched;k++)
        Jsqr[i][j] += J[i][k]*J[k][j];
        }

   /* multiplies three matrices together in two separate steps -- so sue me */
   for(i=0;i<NumMatched;i++)
      for(k=0;k<Dim;k++) {
         Intermediate1[i][k]=0.;
	 for(j=0;j<NumMatched;j++)
	    Intermediate1[i][k] += Jsqr[i][j]*MatchedData2[j][k];
	    }

   for(m=0;m<Dim;m++)
      for(n=0;n<Dim;n++) {
         Intermediate2[m][n]=0.;
	 for(j=0;j<NumMatched;j++)
	    Intermediate2[m][n] += MatchedData1[j][m]*Intermediate1[j][n];
	    }

   
#ifdef DEBUG_SVD
   printf("Matrix for SVD\n");
   for(m=0;m<Dim;m++) {
      for(n=0;n<Dim;n++) printf("%f ",Intermediate2[m][n]);
      printf("\n");
      }
#endif

   /* kludging for Numerical Recipes ... */
   for(m=0;m<Dim;m++) for(n=0;n<Dim;n++) a[m+1][n+1] = Intermediate2[m][n]; 

   /* nuts: I made a,w,v globals to get interface to SVD to work
   svdcmp(a,Dim,Dim,w,v);
   */
   svdcmp(Dim,Dim);

   /* Calc optimal rot/reflect matrix */
   for(m=0;m<Dim;m++) 
      for(n=0;n<Dim;n++) { 
         T[m][n]=0.;
         for(k=0;k<Dim;k++) 
            T[m][n] += v[m+1][k+1]*a[n+1][k+1];
	    }

   /* Calc optimal scale parameter */
	 for(j=0;j<Dim;j++)
	    for(k=0;k<Dim;k++) {
	      Denom[j][k]=0.;
              for(i=0;i<NumMatched;i++)
                 Denom[j][k] += MatchedData2[i][j]*Intermediate1[i][k];
		 }
	 TraceDenominator=0.;
	 for(j=0;j<Dim;j++) TraceDenominator += Denom[j][j];

         for(i=0;i<NumMatched;i++) 
   	    for(k=0;k<Dim;k++) {
	    Intermediate3[i][k]=0.;
   	       for(j=0;j<Dim;j++)
	          Intermediate3[i][k]+=MatchedData2[i][j]*T[j][k];
		  }

         for(i=0;i<NumMatched;i++) 
   	    for(k=0;k<Dim;k++) {
	    Intermediate4[i][k]=0.;
   	       for(j=0;j<NumMatched;j++)
	          Intermediate4[i][k]+=Jsqr[i][j]*Intermediate3[j][k];
		  }

         for(i=0;i<Dim;i++) 
   	    for(k=0;k<Dim;k++) {
	       Numerator[i][k]=0.;
   	       for(j=0;j<NumMatched;j++)
	          Numerator[i][k]+=MatchedData1[j][i]*Intermediate4[j][k];
		  }

	 TraceNumerator=0.;
	 for(j=0;j<Dim;j++)  TraceNumerator+= Numerator[j][j];
	 Scale=TraceNumerator/TraceDenominator;


         for(i=0;i<NumMatched;i++) 
   	    for(k=0;k<Dim;k++) {
	    Intermediate5[i][k]=0.;
   	       for(j=0;j<Dim;j++)
	          Intermediate5[i][k]+=MatchedData2[i][j]*T[j][k];
		  }
         for(i=0;i<NumMatched;i++) 
   	    for(k=0;k<Dim;k++) 
	       Intermediate6[i][k]=(MatchedData1[i][k]-Scale*Intermediate5[i][k])/NumMatched;

   	  for(k=0;k<Dim;k++) {
	      Translation[k]=0.;
              for(i=0;i<NumMatched;i++) Translation[k]+=Intermediate6[i][k];
	      }
   }
   else if(!ScaleFlag) {
   /* create J matrix */
   OneOverN=1./NumMatched;
   for(i=0;i<NumMatched;i++)
      for(j=0;j<NumMatched;j++){
	 if(i==j) J[i][j]=1;
	 else J[i][j]=0;
	 J[i][j] -=OneOverN;
	 }

   /* multiplies three matrices together in two separate steps -- so sue me */
   for(i=0;i<NumMatched;i++)
      for(k=0;k<Dim;k++) {
         Intermediate2[i][k]=0.;
	 for(j=0;j<NumMatched;j++)
	    Intermediate2[i][k] += J[i][j]*MatchedData2[j][k];
	    }

   for(i=0;i<NumMatched;i++)
      for(k=0;k<Dim;k++) {
         Intermediate1[i][k]=0.;
	 for(j=0;j<NumMatched;j++)
	    Intermediate1[i][k] += J[i][j]*MatchedData1[j][k];
	    }

   for(m=0;m<Dim;m++)
      for(n=0;n<Dim;n++) {
         Intermediate3[m][n]=0.;
	 for(j=0;j<NumMatched;j++)
	    Intermediate3[m][n] += Intermediate2[j][m]*Intermediate1[j][n];
	    }

   
#ifdef DEBUG_SVD
   printf("Matrix for SVD\n");
   for(m=0;m<Dim;m++) {
      for(n=0;n<Dim;n++) printf("%f ",Intermediate3[m][n]);
      printf("\n");
      }
#endif

   /* kludging for Numerical Recipes ... */
   for(m=0;m<Dim;m++) for(n=0;n<Dim;n++) a[m+1][n+1] = Intermediate3[n][m]; 

   /* nuts: I made a,w,v globals to get interface to SVD to work
   svdcmp(a,Dim,Dim,w,v);
   */
   svdcmp(Dim,Dim);

   /* Calc optimal rot/reflect matrix */
   for(m=0;m<Dim;m++) 
      for(n=0;n<Dim;n++) { 
         T[m][n]=0.;
         for(k=0;k<Dim;k++) 
            T[m][n] += v[m+1][k+1]*a[n+1][k+1];
	    }


         for(i=0;i<NumMatched;i++) 
   	    for(k=0;k<Dim;k++) {
	    Intermediate5[i][k]=0.;
   	       for(j=0;j<Dim;j++)
	          Intermediate5[i][k]+=MatchedData2[i][j]*T[j][k];
		  }
         for(i=0;i<NumMatched;i++) 
   	    for(k=0;k<Dim;k++) 
	       Intermediate6[i][k]=(MatchedData1[i][k]-Scale*Intermediate5[i][k])/NumMatched;

   	  for(k=0;k<Dim;k++) {
	      Translation[k]=0.;
              for(i=0;i<NumMatched;i++) Translation[k]+=Intermediate6[i][k];
	      }
   }        
#ifdef VERBOSE_PROCRUSTES
   printf("Transformations to change MatchedData2 into MatchedData1 follow:\n\n");
   /*
   printf("MatchedData1=Scale*Rot*MatchedData2+Trans\n");
   */
   printf("Optimal Rotation/Reflection Matrix T:\n");
   for(m=0;m<Dim;m++)  {
      for(n=0;n<Dim;n++) printf("%f ",T[m][n]);
      printf("\n");
      }
   printf("Optimal Scale S= %f\n",Scale);
   printf("Optimal Translation:\n");
   for(k=0;k<Dim;k++) 
   printf("%f ",Translation[k]); printf("\n");
#endif

   /* transform the matched Data2 into Data1 */
   for(i=0;i<NumMatched;i++)
      for(j=0;j<Dim;j++) {
         Transform2Matched[i][j]=0.;
         for(k=0;k<Dim;k++)
            Transform2Matched[i][j] += Scale*MatchedData2[i][k]*T[k][j];
      }
   for(i=0;i<NumMatched;i++)
      for(j=0;j<Dim;j++)
         Transform2Matched[i][j] += Translation[j];

   for(i=0;i<NumMatched;i++)
      for(j=0;j<Dim;j++) {
          tmp = (Transform2Matched[i][j]-MatchedData1[i][j]);
	  LMS += tmp*tmp;
	  }


#ifdef VERBOSE_PROCRUSTES
   printf("\nMatchedData1 MatchedData2\n");
   for(i=0;i<NumMatched;i++) {
      /*
      printf("%s ",MatchedData1ID[i]);
      */
      for(j=0;j<Dim;j++) printf("%f ",MatchedData1[i][j]); printf("\n");
      /*
      printf("%s ",MatchedData2ID[i]);
      */
      for(j=0;j<Dim;j++) printf("%f ",Transform2Matched[i][j]); printf("\n\n");
      }
#endif
   return(LMS);
}

/*
   ----------------------------------------------------------------------------
   Follows is Numerical Recipes SVD code that I just stuck in here so as
   not to have multiple files
   ---------------------------------------------------------------------------
*/
static float at,bt,ct;
#define PYTHAG(a,b) ((at=fabs(a)) > (bt=fabs(b)) ? \
(ct=bt/at,at*sqrt(1.0+ct*ct)) : (bt ? (ct=at/bt,bt*sqrt(1.0+ct*ct)): 0.0))

static float maxarg1,maxarg2;
#define MAX(a,b) (maxarg1=(a),maxarg2=(b),(maxarg1) > (maxarg2) ?\
        (maxarg1) : (maxarg2))
#define SIGN(a,b) ((b) >= 0.0 ? fabs(a) : -fabs(a))


/* nuts: I just made a,w,v globals to get interface to svd to work simply, so
         delete them here as arguments
*/
/*
void svdcmp(a,m,n,w,v)
float **a,*w,**v;
*/

void svdcmp(m,n)
int m,n;
{
        int flag,i,its,j,jj,k,l,nm;
        float c,f,h,s,x,y,z;
        float anorm=0.0,g=0.0,scale=0.0;
        float *rv1,*vector();
        void nrerror(),free_vector();

        if (m < n) nrerror("SVDCMP: You must augment A with extra zero rows");
        rv1=vector(1,n);
        for (i=1;i<=n;i++) {
                l=i+1;
                rv1[i]=scale*g;
                g=s=scale=0.0;
                if (i <= m) {
                        for (k=i;k<=m;k++) scale += fabs(a[k][i]);
                        if (scale) {
                                for (k=i;k<=m;k++) {
                                        a[k][i] /= scale;
                                        s += a[k][i]*a[k][i];
                                }
                                f=a[i][i];
                                g = -SIGN(sqrt(s),f);
                                h=f*g-s;
                                a[i][i]=f-g;
                                if (i != n) {
                                        for (j=l;j<=n;j++) {
                                                for (s=0.0,k=i;k<=m;k++) s += a[
k][i]*a[k][j];
                                                f=s/h;
                                                for (k=i;k<=m;k++) a[k][j] += f*
a[k][i];
                                        }
                                }
                                for (k=i;k<=m;k++) a[k][i] *= scale;
                        }
                }
                w[i]=scale*g;
                g=s=scale=0.0;
                if (i <= m && i != n) {
                        for (k=l;k<=n;k++) scale += fabs(a[i][k]);
                        if (scale) {
                                for (k=l;k<=n;k++) {
                                        a[i][k] /= scale;
                                        s += a[i][k]*a[i][k];
                                }
                                f=a[i][l];
                                g = -SIGN(sqrt(s),f);
                                h=f*g-s;
                                a[i][l]=f-g;
                                for (k=l;k<=n;k++) rv1[k]=a[i][k]/h;
                                if (i != m) {
                                        for (j=l;j<=m;j++) {
                                            for (s=0.0,k=l;k<=n;k++) s += a[ j][k]*a[i][k];
                                                for (k=l;k<=n;k++) a[j][k] += s*
rv1[k];
                                        }
                                }
                                for (k=l;k<=n;k++) a[i][k] *= scale;
                        }
                }
                anorm=MAX(anorm,(fabs(w[i])+fabs(rv1[i])));
        }
        for (i=n;i>=1;i--) {
                if (i < n) {
                        if (g) {
                                for (j=l;j<=n;j++)
                                        v[j][i]=(a[i][j]/a[i][l])/g;
                                for (j=l;j<=n;j++) {
                                        for (s=0.0,k=l;k<=n;k++) s += a[i][k]*v[
k][j];
                                        for (k=l;k<=n;k++) v[k][j] += s*v[k][i];
                                }
                        }
                        for (j=l;j<=n;j++) v[i][j]=v[j][i]=0.0;

                }
                v[i][i]=1.0;
                g=rv1[i];
                l=i;
        }
        for (i=n;i>=1;i--) {
                l=i+1;
                g=w[i];
                if (i < n)
                        for (j=l;j<=n;j++) a[i][j]=0.0;
                if (g) {
                        g=1.0/g;
                        if (i != n) {
                                for (j=l;j<=n;j++) {
                                        for (s=0.0,k=l;k<=m;k++) s += a[k][i]*a[
k][j];
                                        f=(s/a[i][i])*g;
                                        for (k=i;k<=m;k++) a[k][j] += f*a[k][i];
                                }
                        }
                        for (j=i;j<=m;j++) a[j][i] *= g;
                } else {
                        for (j=i;j<=m;j++) a[j][i]=0.0;
                }
                ++a[i][i];
        }
        for (k=n;k>=1;k--) {
                for (its=1;its<=30;its++) {
                        flag=1;
                        for (l=k;l>=1;l--) {
                                nm=l-1;
                                if (fabs(rv1[l])+anorm == anorm) {
                                        flag=0;
                                        break;
                                }
                                if (fabs(w[nm])+anorm == anorm) break;
                        }
                        if (flag) {
                                c=0.0;
                                s=1.0;
                                for (i=l;i<=k;i++) {
                                        f=s*rv1[i];
                                        if (fabs(f)+anorm != anorm) {
                                                g=w[i];

                                                h=PYTHAG(f,g);
                                                w[i]=h;
                                                h=1.0/h;
                                                c=g*h;
                                                s=(-f*h);
                                                for (j=1;j<=m;j++) {
                                                        y=a[j][nm];
                                                        z=a[j][i];
                                                        a[j][nm]=y*c+z*s;
                                                        a[j][i]=z*c-y*s;
                                                }
                                        }
                                }
                        }
                        z=w[k];
                        if (l == k) {
                                if (z < 0.0) {
                                        w[k] = -z;
                                        for (j=1;j<=n;j++) v[j][k]=(-v[j][k]);
                                }
                                break;
                        }
                        if (its == 30) nrerror("No convergence in 30 SVDCMP iterations");
                        x=w[l];
                        nm=k-1;
                        y=w[nm];
                        g=rv1[nm];
                        h=rv1[k];
                        f=((y-z)*(y+z)+(g-h)*(g+h))/(2.0*h*y);
                        g=PYTHAG(f,1.0);
                        f=((x-z)*(x+z)+h*((y/(f+SIGN(g,f)))-h))/x;
                        c=s=1.0;
                        for (j=l;j<=nm;j++) {
                                i=j+1;
                                g=rv1[i];
                                y=w[i];
                                h=s*g;
                                g=c*g;
                                z=PYTHAG(f,h);
                                rv1[j]=z;
                                c=f/z;
                                s=h/z;
                                f=x*c+g*s;
                                g=g*c-x*s;
                                h=y*s;
                                y=y*c;
                                for (jj=1;jj<=n;jj++) {
                                        x=v[jj][j];
                                        z=v[jj][i];
                                        v[jj][j]=x*c+z*s;
                                        v[jj][i]=z*c-x*s;
                                }
                                z=PYTHAG(f,h);
                                w[j]=z;
                                if (z) {
                                        z=1.0/z;
                                        c=f*z;
                                        s=h*z;
                                }
                                f=(c*g)+(s*y);
                                x=(c*y)-(s*g);
                                for (jj=1;jj<=m;jj++) {
                                        y=a[jj][j];
                                        z=a[jj][i];

                                        a[jj][j]=y*c+z*s;
                                        a[jj][i]=z*c-y*s;
                                }
                        }
                        rv1[l]=0.0;
                        rv1[k]=f;
                        w[k]=x;
                }
        }
        free_vector(rv1,1,n);
}

#undef SIGN
#undef MAX
#undef PYTHAG


void nrerror(error_text)
char error_text[];
{
        void exit();

        fprintf(stderr,"Numerical Recipes run-time error...\n");
        fprintf(stderr,"%s\n",error_text);
        fprintf(stderr,"...now exiting to system...\n");
        exit(1);
}

float *vector(nl,nh)
int nl,nh;
{
        float *v;

        v=(float *)malloc((unsigned) (nh-nl+1)*sizeof(float));
        if (!v) nrerror("allocation failure in vector()");
        return v-nl;
}


void free_vector(v,nl,nh)
float *v;
int nl,nh;
{
        free((char*) (v+nl));
	}


   sort(Index,Value,n)
int Index[];
float Value[];
int n;
   {
   int i,j,temp_i;
   float temp_r;

   for(i=0; i<n-1; ++i)
      for(j=i+1; j<n; ++j)
          if(Value[i]>Value[j]) {
            temp_i=Index[i];
            temp_r=Value[i];
            Index[i]=Index[j];
            Value[i]=Value[j];
            Index[j]=temp_i;
            Value[j]=temp_r;
         }
   }

ReadData(fn,Data,DataID)
FILE *fn;
float Data[][MAX_DIM];
char DataID[][BUFSIZE];
{
   int num=0,d;

   while(!feof(fn)) {
	 fscanf(fn,"%s",&DataID[num]);
	 for(d=0;d<Dim;d++) {
	    fscanf(fn,"%f ",&Data[num][d]);
	    if(feof(fn)) break;
	    }
   #ifdef LOCKFLAG_FORMAT
	 fscanf(fn,"%c",&LockFlag); if(feof(fn)) break;
   #endif
	 num++;
	 } 
   return(num);

}
