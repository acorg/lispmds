#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define MAX_EXAMPLES 4000
#define MAX_DIM 50
#define BUFSIZE 1024

/* made this global to get svd interfacing right */
float a[MAX_DIM][MAX_DIM],v[MAX_DIM][MAX_DIM],w[MAX_DIM];


float Data1[MAX_EXAMPLES][MAX_DIM],Data2[MAX_EXAMPLES][MAX_DIM];
char Data1ID[MAX_EXAMPLES][BUFSIZE],Data2ID[MAX_EXAMPLES][BUFSIZE];
float MatchedData1[MAX_EXAMPLES][MAX_DIM],MatchedData2[MAX_EXAMPLES][MAX_DIM];
char MatchedData1ID[MAX_EXAMPLES][BUFSIZE],MatchedData2ID[MAX_EXAMPLES][BUFSIZE];
float Transform2[MAX_EXAMPLES][MAX_DIM];
float Transform2Matched[MAX_EXAMPLES][MAX_DIM];
float Intermediate1[MAX_EXAMPLES][MAX_DIM],Intermediate2[MAX_EXAMPLES][MAX_DIM];
float Intermediate3[MAX_EXAMPLES][MAX_DIM],Intermediate4[MAX_EXAMPLES][MAX_DIM];
float Intermediate5[MAX_EXAMPLES][MAX_DIM],Intermediate6[MAX_EXAMPLES][MAX_DIM];
float Translation[MAX_DIM];
float J[MAX_EXAMPLES][MAX_EXAMPLES],OneOverN;
float Jsqr[MAX_EXAMPLES][MAX_EXAMPLES];
float T[MAX_DIM][MAX_DIM];
float Scale=1., RMS=0.,tmp;
float Denom[MAX_DIM][MAX_DIM],TraceDenominator=0.;
float Numerator[MAX_DIM][MAX_DIM],TraceNumerator=0.;
float dist,AveDist,SD;
char LockFlag,Name1[BUFSIZE],Name2[BUFSIZE],junk[BUFSIZE];
char MatchList1[MAX_EXAMPLES][BUFSIZE], MatchList2[MAX_EXAMPLES][BUFSIZE];
char Name_1_2_coords[BUFSIZE], Name_1_labels[BUFSIZE], Name_2_labels[BUFSIZE];
char Name_1_2_arrows[BUFSIZE];

main(argc,argv)
int argc;
char *argv[];
{
FILE *fn1, *fn2;
int N1,N2,NumMatched=0,Dim,ID,NumOnMatchList;
int d,i,j,k,m,n,cntr,ScaleFlag;
register int num1=0,num2=0;


FILE *fn_procrustes_1_2_gnu, *fn_procrustes_1_labels,*fn_procrustes_2_labels;
FILE *fn_procrustes_1_2_arrows;
FILE *fn_diagnostics;
void svdcmp();

/* PURPOSE: to perform a rigid body transformation to get best RMS error
overlap of two files. Accepts a MatchList of names
Name1a Name2a
Name1b Name2b
...
Name1  Name2
so that only these coords are involved in deriving the transformation,
not all of them. The transf. is however applied to all.

Procrustes1.c is same as Procustes.c except that now:
(1) it opens a file $DataFile1.$DataFile2.procrustes.coords.gnu which contains
the coordinates of the second file after it has been through the procrustes
transformation to the first file.
(2) it opens a file  $DataFile1.procrustes.labels that contains gnuplot
format labels for the first file
(3) it opens a file  $DataFile2.procrustes.labels that contains gnuplot
format labels for the second file
(4) it opens a file $DataFile1.$DataFile2.procrustes.arrows.gnu that contains
gnuplot format "arrow" or "line" syntax to connect the corresponding
points from DataFile1 and DataFile2.

Procrustes2.c is same as Procustes1.c except that now:
(1) it has a command line option, "Scale", to enable/disable scaling.
    enable: Scale=1 disable: Scale=0

*/

   if(argc!=8) {
      fprintf(stderr,"Use: %s DataFile1 DataFile2 Dimension MatchList NamesCoordsOutputFilename DiagnosticsFilename Scaling[1/0]\n",
      argv[0]);
      exit(1);
   }

   if((fn_diagnostics=fopen(argv[6],"w"))==NULL) {
      fprintf(stderr,"Can't open %s\n",argv[6]);
      exit(1);
      }

   Dim=atoi(argv[3]); ScaleFlag=atoi(argv[7]);
   printf("DataFile1=%s\nDataFile2=%s\nDimension=%d\n\n",argv[1], argv[2],Dim);
   fprintf(fn_diagnostics,"DataFile1 %s\nDataFile2 %s\nDimension %d\n\n",argv[1], argv[2],Dim);


   /* open the various files for output */ 
   /*
   sprintf(Name_1_2_coords,"%s.AND.%s.procrustes.coords.gnu",argv[1],argv[2]);
   */

   /* sprintf(Name_1_2_coords,"%s_%s.procrustes.coords.gnu",argv[1],argv[2]);    derek replaced */
   sprintf(Name_1_2_coords,"%s",argv[5]);
   if((fn_procrustes_1_2_gnu=fopen(Name_1_2_coords,"w"))==NULL) {
      fprintf(stderr,"Can't open %s\n",Name_1_2_coords);
      exit(1);
      }
   /*
   derek commented out
   sprintf(Name_1_labels,"%s.procrustes.labels.gnu",argv[1]);
   if((fn_procrustes_1_labels=fopen(Name_1_labels,"w"))==NULL) {
      fprintf(stderr,"Can't open %s\n",Name_1_labels);
      exit(1);
      }
   sprintf(Name_2_labels,"%s.procrustes.labels.gnu",argv[2]);
   if((fn_procrustes_2_labels=fopen(Name_2_labels,"w"))==NULL) {
      fprintf(stderr,"Can't open %s\n",Name_2_labels);
      exit(1);
      }
   sprintf(Name_1_2_arrows,"%s_%s.procrustes.arrows.gnu",argv[1],argv[2]);
   if((fn_procrustes_1_2_arrows=fopen(Name_1_2_arrows,"w"))==NULL) {
      fprintf(stderr,"Can't open %s\n",Name_1_2_arrows);
      exit(1);
      }
   */

   if((fn1=fopen(argv[1],"r"))==NULL) {
      fprintf(stderr,"Can't open %s\n",argv[1]);
      exit(1);
      }
   while(!feof(fn1)) {
      fscanf(fn1,"%s",&Data1ID[num1]);
      for(d=0;d<Dim;d++) {
         fscanf(fn1,"%f ",&Data1[num1][d]);
         if(feof(fn1)) break;
         }
	 /*
         fscanf(fn1,"%c",&LockFlag);
         if(feof(fn1)) break;
	 */
      num1++;
      } fclose(fn1);
	 
   if((fn2=fopen(argv[2],"r"))==NULL) {
      fprintf(stderr,"Can't open %s\n",argv[2]);
      exit(1);
      }
   while(!feof(fn2)) {
      fscanf(fn2,"%s",&Data2ID[num2]);
      for(d=0;d<Dim;d++) {
         fscanf(fn2,"%f ",&Data2[num2][d]);
         if(feof(fn2)) break;
         }
	 /*
         fscanf(fn2,"%c",&LockFlag);
         if(feof(fn2)) break;
	 */
      num2++;
      } fclose(fn2);
   
   cntr=0;
   if((fn2=fopen(argv[4],"r"))==NULL) {
      fprintf(stderr,"Can't open %s\n",argv[4]);
      exit(1);
      }
   while(!feof(fn2)) {
      fscanf(fn2,"%s",MatchList1[cntr]);
      if(feof(fn2)) break;
      cntr++;
      } NumOnMatchList=cntr;
   fprintf(stderr,"MATCH=%d\n",cntr);
   fprintf(fn_diagnostics,"MATCH %d\n",cntr);

   fprintf(stderr,"Read: num1= %d num2= %d examples, num=%d matches\n",num1,num2,NumOnMatchList);
   fprintf(fn_diagnostics,"Read  num1  %d num2  %d examples  num %d matches\n",num1,num2,NumOnMatchList);
   if(NumOnMatchList>num1) {
      fprintf(stderr,"NumOnMatchList= %d is > num1= %d\n",NumOnMatchList,num1);
      exit(1);
      }
   if(NumOnMatchList>num2) {
      fprintf(stderr,"NumOnMatchList= %d is > num2= %d\n",NumOnMatchList,num2);
      exit(1);
      }

   for(cntr=0;cntr<NumOnMatchList;cntr++) {

      for(i=0;i<num1;i++)
	 if(!strcmp(Data1ID[i],MatchList1[cntr])) break;
      if(i==num1)
         {fprintf(stderr,"MatchList1[%d]= %s not found in first file\n",
			 cntr,MatchList1[cntr]); }

      for(j=0;j<num2;j++)
	 if(!strcmp(Data2ID[j],MatchList1[cntr])) break;
      if(i==num2)
         {fprintf(stderr,"MatchList1[%d]= %s not found in first file\n",
			 cntr,MatchList1[cntr]); }

      if(i!=num1&&j!=num2) {
         strcpy(MatchedData1ID[NumMatched],Data1ID[i]);
         strcpy(MatchedData2ID[NumMatched],Data2ID[j]);
	 for(k=0;k<Dim;k++) MatchedData1[NumMatched][k]=Data1[i][k];
	 for(k=0;k<Dim;k++) MatchedData2[NumMatched][k]=Data2[j][k];
	 NumMatched++;
         }

      }

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
   
   printf("Transformations to change MatchedData2 into MatchedData1 follow:\n\n");
   fprintf(fn_diagnostics, "Transformations to change MatchedData2 into MatchedData1 follow \n\n");
   /*
   printf("MatchedData1=Scale*Rot*MatchedData2+Trans\n");
   */
   printf("Optimal Rotation/Reflection Matrix T:\n");
   fprintf(fn_diagnostics, "Optimal-Rotation/Reflection-Matrix ");
   for(m=0;m<Dim;m++)  {
      fprintf(fn_diagnostics, "(");
      for(n=0;n<Dim;n++) {
	  printf("%f ",T[m][n]);
	  fprintf(fn_diagnostics, "%f ",T[m][n]);
      }
      printf("\n");
      fprintf(fn_diagnostics, ") ");
   }
   fprintf(fn_diagnostics, "\n");
   printf("\nOptimal Scale S= %f\n",Scale);
   fprintf(fn_diagnostics, "\nOptimal-Scale  %f\n",Scale);
   printf("\nOptimal Translation:\n");
   fprintf(fn_diagnostics, "\nOptimal-Translation ");
   for(k=0;k<Dim;k++) {
       printf("%f ",Translation[k]);
       fprintf(fn_diagnostics, "%f ",Translation[k]);
   }
   printf("\n");
   fprintf(fn_diagnostics, "\n");

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
	  RMS += tmp*tmp;
	  }
   RMS=sqrt(RMS/NumMatched);
   printf("\nRMS=%f\n",RMS);
   fprintf(fn_diagnostics, "\nRMS %f\n",RMS);



   /* transform all of Data2 into Data1 */
   for(i=0;i<num2;i++)
      for(j=0;j<Dim;j++) {
         Transform2[i][j]=0.;
         for(k=0;k<Dim;k++)
            Transform2[i][j] += Scale*Data2[i][k]*T[k][j];
      }
   for(i=0;i<num2;i++)
       for(j=0;j<Dim;j++) {
         Transform2[i][j] += Translation[j];
       }


   /* derek debug  transform all of Data2 into Data1 
   for(i=0;i<num2;i++)
      for(j=0;j<Dim;j++) {
         Transform2[i][j]=0.;
         for(k=0;k<Dim;k++) {
            Transform2[i][j] += Data2[i][k]*T[k][j];
	 }
	 printf("data2[%d][%d] pre %f    rotation %f",
		i,j,
		Data2[i][j],
		Transform2[i][j]);
	 Transform2[i][j] = Scale*Transform2[i][j];
	 printf("   scale %f", Transform2[i][j]);
         Transform2[i][j] += Translation[j];
	 printf("   translation %f\n", Transform2[i][j]);
      }
   */

#ifdef OLD_FILE_OUTPUT
/*
this is the old file output from Procrustes.c It outputs rmf
format versions of the procrustes transformation of the two input files
not a feature Derek would want, hence ifdef'd out here.
*/
   sprintf(Name1,"%s.procrustes",argv[1]);
   if((fn1=fopen(Name1,"w"))==NULL) {
      fprintf(stderr,"Can't open %s\n",Name1);
      exit(1);
      }
   sprintf(Name2,"%s.procrustes",argv[2]);
   if((fn2=fopen(Name2,"w"))==NULL) {
      fprintf(stderr,"Can't open %s\n",Name2);
      exit(1);
      }

   fprintf(fn1,"n_dim=%d;\n",Dim);
   fprintf(fn1,"n_rank=0;\n");
   fprintf(fn1,"energy=0;\n");
   for(i=0;i<num1;i++) {
      fprintf(fn1,"C %s {",Data1ID[i]);
      for(j=0;j<Dim-1;j++) fprintf(fn1,"%f,",Data1[i][j]);
      fprintf(fn1,"%f};\n",Data1[i][j]);
      }
   fprintf(fn2,"n_dim=%d;\n",Dim);
   fprintf(fn2,"n_rank=0;\n");
   fprintf(fn2,"energy=0;\n");
   for(i=0;i<num2;i++) {
      fprintf(fn2,"C %s {",Data2ID[i]);
      for(j=0;j<Dim-1;j++) fprintf(fn2,"%f,",Transform2[i][j]);
      fprintf(fn2,"%f};\n",Transform2[i][j]);
   }
   fclose(fn1);fclose(fn2);
#endif

   AveDist=0; SD=0.;

#ifdef OLD_STDOUT_OUTPUT
/*
this is the old stdout output from Procrustes.c
not sure why I had this, but can turn this back on easily if needed.
*/
   printf("\nMatchedData1 MatchedData2\n");
#endif

   for(i=0;i<NumMatched;i++) {
      dist=0.;

#ifdef OLD_STDOUT_OUTPUT
      printf("%s ",MatchedData1ID[i]);
      for(j=0;j<Dim;j++) printf("%f ",MatchedData1[i][j]);
      printf("\n");
      printf("%s ",MatchedData2ID[i]);
      for(j=0;j<Dim;j++) printf("%f ",Transform2Matched[i][j]);
#endif

      for(j=0;j<Dim;j++)
      dist += (MatchedData1[i][j]-Transform2Matched[i][j])*(MatchedData1[i][j]-Transform2Matched[i][j]); dist=sqrt(dist);

#ifdef OLD_STDOUT_OUTPUT
      printf("\ndist= %f\n\n",dist);
#endif

      AveDist+=dist;
      SD += dist*dist;
   } fflush(stdout);
   AveDist/=NumMatched;
   SD /= NumMatched; SD -= AveDist*AveDist;

   printf("AveDist= %f SD= %f\n\n",AveDist,sqrt(SD));
   fprintf(fn_diagnostics, "AveDist  %f\nSD  %f\n\n",AveDist,sqrt(SD));


      /* print out file 2 matched to file 1 coordinates */
#ifdef PRINTFILE1
      /* first print out all of file1, then print just the matchs of file2 */
   for(i=0;i<NumMatched;i++) {
      fprintf(fn_procrustes_1_2_gnu,"%s ",Data1ID[i]);
      for(j=0;j<Dim;j++)
      fprintf(fn_procrustes_1_2_gnu,"%f ",Data1[i][j]);
      fprintf(fn_procrustes_1_2_gnu,"\n");
      }
#endif

   /* here are the matches of file2 */
   for(i=0;i<NumMatched;i++) {
      fprintf(fn_procrustes_1_2_gnu,"%s ",MatchedData2ID[i]);
      for(j=0;j<Dim;j++)
      fprintf(fn_procrustes_1_2_gnu,"%f ",Transform2Matched[i][j]);
      fprintf(fn_procrustes_1_2_gnu,"\n");
      }


   for(i=0;i<NumMatched;i++) {
/*              !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!         */
/*       NOTE THAT THE ARROWS SECTION HERE IS HARDWIRED FOR TWO DIMENSIONS   */
/*              !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!         */

      /* print out file 2 matched to file 1 arrows */
       /* derek fprintf(fn_procrustes_1_2_arrows,"set arrow from %f,%f to %f,%f\n",Transform2Matched[i][0],Transform2Matched[i][1],MatchedData1[i][0],MatchedData1[i][1]); */




/*              !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!         */
/*       NOTE THAT THE LABELS SECTION HERE IS HARDWIRED FOR TWO DIMENSIONS   */
/*              !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!         */
      /* print out file 1 labels, a gnuplot tag has to be >0 */ 
      /* derek fprintf(fn_procrustes_1_labels,"set label %d \"%s\" at %f,%f\n",i+1,MatchedData1ID[i],MatchedData1[i][0],MatchedData1[i][1]); */

      /* print out file 2 labels, a gnuplot tag has to be >0 */ 
      /* derek fprintf(fn_procrustes_2_labels,"set label %d \"%s\" at %f,%f\n",i+1,MatchedData2ID[i],Transform2Matched[i][0],Transform2Matched[i][1]); */


      }

   fclose(fn_procrustes_1_2_gnu);

   /* derek
   fclose(fn_procrustes_1_labels);
   fclose(fn_procrustes_2_labels); fclose(fn_procrustes_1_2_arrows);
   */

}


static float at,bt,ct;
#define PYTHAG(a,b) ((at=fabs(a)) > (bt=fabs(b)) ? \
(ct=bt/at,at*sqrt(1.0+ct*ct)) : (bt ? (ct=at/bt,bt*sqrt(1.0+ct*ct)): 0.0))

static float maxarg1,maxarg2;
#define MAX(a,b) (maxarg1=(a),maxarg2=(b),(maxarg1) > (maxarg2) ?\
        (maxarg1) : (maxarg2))
#define SIGN(a,b) ((b) >= 0.0 ? fabs(a) : -fabs(a))


/* nuts: I just made a,w,v globals to get interface to svd to work
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
                                                for (s=0.0,k=i;k<=m;k++) s += a[k][i]*a[k][j];
                                                f=s/h;
                                                for (k=i;k<=m;k++) a[k][j] += f*a[k][i];
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
                                                for (k=l;k<=n;k++) a[j][k] += s*rv1[k];
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
                                        for (s=0.0,k=l;k<=n;k++) s += a[i][k]*v[k][j];
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
                                        for (s=0.0,k=l;k<=m;k++) s += a[k][i]*a[k][j];
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


