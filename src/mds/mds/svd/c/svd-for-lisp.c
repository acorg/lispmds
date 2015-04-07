#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include "nrutil.c"
#include "nrutil.h"
#include "svdcmp.c"
#include "svbksb.c"
#include "pythag.c"   /* derek had to add to get a compile */

#define BUFSIZE 2700    /* derek 2900 here and below causes a seg fault when run on cmd line, and 2800 when run from lisp */
#define MAXCOLS  2700   /* derek */
#define TRUE 1
#define FALSE 0

int Size,DataHeader,SquareIt;
float InputMatrix[MAXCOLS][MAXCOLS];
float Sum,Sum_i[MAXCOLS],Sum_j[MAXCOLS];
float **Matrix, **Matrix_inverse, **Check, **u, **v, *w, *w_inverse;

main(argc,argv)
   int argc;
   char *argv[];
{
   char buf[BUFSIZE],Name[BUFSIZE];
   char Label[MAXCOLS][BUFSIZE];
   register int i,j,k,l,col,cntr=0;
   int PutativeHessianModesFlag;
   int I,Alpha,J,Beta,p,S,L,Dim,d;
   float Threshold,Dot,DiffSqr,DiffNN=0,DiffNZ=0,DiffZN=0,DiffZZ=0;
   float Cumulative=0.,Stress=0,dsquare,tmp;
   int OutputDim;
   FILE *fn;

/* PURPOSE:
This code reads in a distance_squared matrix as a full symmetric matrix with
floats separated by spaces on each row. It then compute the
Delta transformation of the distance_squared matrix, svd's this, and
puts out the svd_reconstructed coordinates of dimension=OutputDim

Same as SVD_DistanceMatrix.bak2.c but have attempted to insert a sort
on the eigenvalues and associated matrices.

Example: 10 points in two dimensions
0.340188 -0.105617
0.283099 0.298440
0.411647 -0.302449
-0.164777 0.268230
-0.222225 0.053970
-0.022603 0.128871
-0.135216 0.013401
0.452230 0.416195
0.135712 0.217297
-0.358397 0.106969
Distance_sqr
0.000000 0.166521 0.043849 0.394751 0.341776 0.186602 0.240174 0.284841 0.146084 0.533214 
0.166521 0.000000 0.377592 0.201506 0.315118 0.122207 0.256235 0.042472 0.028307 0.448178 
0.043849 0.377592 0.000000 0.657939 0.528828 0.374610 0.398820 0.518096 0.346276 0.760591 
0.394751 0.201506 0.657939 0.000000 0.049208 0.039634 0.065812 0.402591 0.092888 0.063494 
0.341776 0.315118 0.528828 0.049208 0.000000 0.045459 0.009216 0.586097 0.154795 0.021352 
0.186602 0.122207 0.374610 0.039634 0.045459 0.000000 0.026015 0.308021 0.032883 0.113237 
0.240174 0.256235 0.398820 0.065812 0.009216 0.026015 0.000000 0.507336 0.114976 0.058565 
0.284841 0.042472 0.518096 0.402591 0.586097 0.308021 0.507336 0.000000 0.139744 0.752737 
0.146084 0.028307 0.346276 0.092888 0.154795 0.032883 0.114976 0.139744 0.000000 0.256316 
0.533214 0.448178 0.760591 0.063494 0.021352 0.113237 0.058565 0.752737 0.256316 0.000000 


*/

   if(argc!=8) {
      fprintf(stderr,"Use: %s MatrixSize Threshold DataHeader(1/0) OutputDim SquareIt MatrixFile LabelFile\n", argv[0]);
      exit(1);
      }
      Size=atoi(argv[1]);
      DataHeader=atoi(argv[3]);
      /* fprintf(stderr,"Size=%d\n",Size);  derek, not for lisp version */
      Threshold=atof(argv[2]);
      OutputDim=atoi(argv[4]);
      SquareIt=atoi(argv[5]);

      if((fn=fopen(argv[6],"r"))==NULL) {
      fprintf(stderr,"Can't open %s\n",argv[6]);
      exit(1);
      }

   /* read data */
   ReadMatrix(fn);
   fclose(fn);

   /* read labels for data */
   cntr=0;
   if(argv[7][0]!='-') {
      if((fn=fopen(argv[7],"r"))==NULL) {
      fprintf(stderr,"Can't open %s\n",argv[7]);
      exit(1);
      }
      while(!feof(fn)) {
	 fscanf(fn,"%s",&Label[cntr]);
	 if(feof(fn)) break;
	 cntr++;
	 } fclose(fn);
	 }

   /* alloc memory space */
   Matrix=matrix(1,Size,1,Size);
   Matrix_inverse=matrix(1,Size,1,Size);
   Check=matrix(1,Size,1,Size);
   v=matrix(1,Size,1,Size);
   u=matrix(1,Size,1,Size);
   w=vector(1,Size);
   w_inverse=vector(1,Size);

/* symmetry/diagonal check
for(i=0;i<Size;i++)
for(j=0;j<Size;j++) 
if(fabs(InputMatrix[i][j]-InputMatrix[j][i])>1e-6)
printf("SymmetryProblem %d %d %f %f\n",i,j,InputMatrix[i][j],InputMatrix[j][i]);
for(i=0;i<Size;i++)
if(fabs(InputMatrix[i][i])>1e-6)
printf("DiagonalProblem %d %d %f\n",i,i,InputMatrix[i][i]);
*/


   /* form Delta matrix here, assuming that distance^2 read in or squared up
      during ReadMatrix
   */
   for(i=0;i<Size;i++) {
      Sum_i[i]=0.;
      for(j=0;j<Size;j++) Sum_i[i]+=InputMatrix[i][j];
      Sum_i[i]/=Size;
      }

   for(j=0;j<Size;j++) {
      Sum_j[j]=0.;
      for(i=0;i<Size;i++) Sum_j[j]+=InputMatrix[i][j];
      Sum_j[j]/=Size;
      }

   Sum=0.;
   for(i=0;i<Size;i++)
      for(j=0;j<Size;j++) 
      Sum+=InputMatrix[i][j];
   Sum/=(Size*Size);


   /* do the svd decomp on the Delta transformed distance_square matrix */
   for(i=1;i<=Size;i++)
      for(j=1;j<=Size;j++)
         u[i][j]=0.5*(InputMatrix[i-1][j-1]-Sum_i[i-1]-Sum_j[j-1]+Sum);

/* u[][] should now be symmetric and tracefree, can check it here:
for(i=1;i<=Size;i++) {
Sum=0;
for(j=1;j<=Size;j++) Sum += u[i][j];
printf("i=%d Sum_jDelta=%f\n",i,Sum);
}
Sum=0;
for(i=1;i<=Size;i++) {
for(j=1;j<=Size;j++) Sum += u[i][j];
}
printf("TotalSumDelta=%f\n",Sum);
for(i=1;i<=Size;i++)
for(j=1;j<=Size;j++)
if(u[i][j]!=u[j][i]) printf("nonsymmetric: %d %d:%d %d\n",i,j,j,i);
*/

   svdcmp(u,Size,Size,w,v);

/*
printf("#Eigenvalues Before Sorting\n");
for(i=1;i<=Size;i++) printf("#w[%d]= %f\n",i,w[i]);
*/

   /* sort eigenvalues, w[j], and associated u[i][j] */
   sort(Size);

   /* calc mds style stress given the OutputDim */
   Stress=0.;
   for(i=1;i<=Size;i++) 
   for(j=i+1;j<=Size;j++) {

      dsquare=0;
      for(d=1;d<=OutputDim;d++) 
      dsquare += (u[i][d]*sqrt(w[d])-u[j][d]*sqrt(w[d]))*(u[i][d]*sqrt(w[d])-u[j][d]*sqrt(w[d]));

      Stress += (sqrt(InputMatrix[i-1][j-1])-sqrt(dsquare))*(sqrt(InputMatrix[i-1][j-1])-sqrt(dsquare));
      }
   /* fprintf(stderr,"OutputDim= %d Stress= %f\n",OutputDim,Stress);   derek, not for lisp version */


/* #define WRITE_GNU    derek, don't want this for lisp version */
#ifdef WRITE_GNU
   sprintf(Name,"%s.dim%s.svd.gnu.coords",argv[6],argv[4]);
   if((fn=fopen(Name,"w"))==NULL) {
      fprintf(stderr,"Can't open %s\n",Name);
	 exit(1);
	    }
   printf("#Reconstructed Coordinates in Dim= %d dimensions\n",OutputDim);
   for(i=1;i<=Size;i++) {
      for(j=1;j<=OutputDim;j++) fprintf(fn,"%f ", u[i][j]*sqrt(w[j]));
      fprintf(fn,"\n");
      }
      fclose(fn);

      if(argv[7][0]!='-') {
      sprintf(Name,"%s.dim%s.svd.gnu.labels",argv[6],argv[4]);
      if((fn=fopen(Name,"w"))==NULL) {
         fprintf(stderr,"Can't open %s\n",Name);
	    exit(1);
	       }
      for(i=1;i<=Size;i++) {
         fprintf(fn,"set label %d \"%s\" at ",i,Label[i-1]); 
	 for(j=1;j<=OutputDim-1;j++) fprintf(fn,"%f,", u[i][j]*sqrt(w[j]));
	 fprintf(fn,"%f\n", u[i][j]*sqrt(w[j]));
	 }
      fclose(fn);
      }
#endif

/* #define WRITE_RMF */    /* derek, don't want this for the lisp version */
#ifdef WRITE_RMF

   sprintf(Name,"%s.dim%s.svd.rmf.coords",argv[6],argv[4]);
   if((fn=fopen(Name,"w"))==NULL) {
      fprintf(stderr,"Can't open %s\n",Name);
	 exit(1);
	    }
   fprintf(fn,"n_dim=%d;\n",OutputDim);
   fprintf(fn,"rank=0;\n");
   fprintf(fn,"energy=0;\n");
   fflush(fn);
   /* write coordinates */
   for(i=1;i<=Size;i++) {
   fprintf(fn,"C %s\t{",Label[i-1]);

   for(j=1;j<=OutputDim-1;j++) fprintf(fn,"%f,", u[i][j]*sqrt(w[j]));
   fprintf(fn,"%f", u[i][OutputDim]*sqrt(w[OutputDim]));

   fprintf(fn,"};\n");

   }
   /* write distances*/
   for(i=0;i<Size;i++)
   for(j=i+1;j<Size;j++)
   fprintf(fn,"A %s,%s %f;\n",Label[i],Label[j],sqrt(InputMatrix[i][j]));
   /*
   fprintf(fn,"A %s,%s %f;\n",Label[i],Label[j],(InputMatrix[i][j]));
   */
#endif


/* print out the nonzero modes (those over Threshold)
for(j=1;j<=Size;j++) {
if(fabs(w[j])>Threshold) {
printf("PutativeNonZeroMode: w[%d]= %f\n",j,w[j]);
for(i=1;i<=Size;i++) printf("%f\n",u[i][j]);
}
} fflush(stdout);
*/


/* turn on various DEBUG Flags, if desired */
/* #define DEBUGW  derek, turn off for lisp version */
#ifdef DEBUGW
/* w arrays holds eigenvalues */
printf("#Eigenvalues\n");
for(i=1;i<=Size;i++) printf("#w[%d]= %f\n",i,w[i]);

printf("#Cumulative Variance\n");
Sum=0.;
for(i=1;i<=Size;i++) Sum += w[i];

for(i=1;i<=Size;i++) {
Cumulative=0.;
for(j=1;j<=i;j++) Cumulative += w[j];
printf("#Cumulative[%d]= %f\n",i,Cumulative/Sum);
}
fflush(stdout);
#endif

#ifdef MDS_STRESS
printf("# MDS-style Stress\n");
/* calc mds style stress given each chosen dimension */
for(Dim=1;Dim<=Size;Dim++) {
Stress=0.;
   for(i=1;i<=Size;i++)
   for(j=i+1;j<=Size;j++) {

      dsquare=0;
      for(d=1;d<=Dim;d++)  {
      tmp=(u[i][d]-u[j][d])*sqrt(w[d]);
      dsquare += tmp*tmp;
      }

      Stress += (InputMatrix[i-1][j-1]-dsquare)*(InputMatrix[i-1][j-1]-dsquare);
/*
printf("dsquare= %f given= %f\n",dsquare,InputMatrix[i-1][j-1]);
*/
      }

printf("#Dim= %d Stress= %f\n",Dim,Stress); fflush(stdout);
}

fflush(stdout);
#endif

#ifdef DEBUGU
/* the classic U and V orthonormal arrays produced by SVD */
printf("DEBUG: final  u\n");
for(i=1;i<=Size;i++) {
for(j=1;j<=Size;j++) printf("%f ",u[i][j]);
printf("\n");
}
fflush(stdout);
#endif

#ifdef DEBUGV
/* the classic U and V orthonormal arrays produced by SVD */
printf("DEBUG: v\n");
for(i=1;i<=Size;i++) {
for(j=1;j<=Size;j++) printf("%f ",v[i][j]);
printf("\n");
}
fflush(stdout);
#endif


#ifdef CHECK_MATRIX
/* Check how well the matrix is reconstructed using only modes > Threshold */
printf("DEBUG: CHECK OF MATRIX\n");
for(i=1;i<=Size;i++) for(j=1;j<=Size;j++) Check[i][j]=0.;

for(i=1;i<=Size;i++)
for(j=1;j<=Size;j++)
for(k=1;k<=Size;k++) {
if(w[k]>Threshold)
Check[i][j] += v[i][k]*w[k]*u[j][k];
}

for(i=1;i<=Size;i++) for(j=1;j<=Size;j++) {
if(InputMatrix[i-1][j-1]>1e-6&&Check[i][j]>1e-6) {
DiffNN+=(InputMatrix[i-1][j-1]-Check[i][j])*(InputMatrix[i-1][j-1]-Check[i][j]);
printf("InputN %f ReconstructedN %f\n",InputMatrix[i-1][j-1],Check[i][j]);
}
if(InputMatrix[i-1][j-1]>1e-6&&Check[i][j]<1e-6) {
DiffNZ+=(InputMatrix[i-1][j-1]-Check[i][j])*(InputMatrix[i-1][j-1]-Check[i][j]);
printf("InputN %f ReconstructedZ %f\n",InputMatrix[i-1][j-1],Check[i][j]);
}
if(InputMatrix[i-1][j-1]<1e-6&&Check[i][j]>1e-6) {
DiffZN+=(InputMatrix[i-1][j-1]-Check[i][j])*(InputMatrix[i-1][j-1]-Check[i][j]);
printf("InputZ %f ReconstructedN %f\n",InputMatrix[i-1][j-1],Check[i][j]);
}
if(InputMatrix[i-1][j-1]<1e-6&&Check[i][j]<1e-6) {
DiffZZ+=(InputMatrix[i-1][j-1]-Check[i][j])*(InputMatrix[i-1][j-1]-Check[i][j]);
printf("InputZ %f ReconstructedZ %f\n",InputMatrix[i-1][j-1],Check[i][j]);
}
}
DiffNN/=(Size*Size);
DiffNZ/=(Size*Size);
DiffZN/=(Size*Size);
DiffZZ/=(Size*Size);
fprintf(stderr,"RMS Diff of InputMatrix_ij and spectral reconstruction:DiffNN= %f DiffNZ= %f DiffZN=%f DiffZZ= %f\n",sqrt(DiffNN),sqrt(DiffNZ),sqrt(DiffZN),sqrt(DiffZZ));

/*
for(i=1;i<=Size;i++) {
for(j=1;j<=Size;j++) printf("%f ",Check[i][j]);
printf("\n");
}
*/
fflush(stdout);
#endif

#ifdef  INVERSE_MATRIX

for(i=1;i<=Size;i++)
for(j=1;j<=Size;j++) {
Matrix_inverse[i][j]=0.;
for(k=1;k<=Size;k++) 
if(w[k]>Threshold)
Matrix_inverse[i][j] += v[i][k]*u[j][k]/w[k];
}
/*
printf("Matrix Inverse\n");
for(i=1;i<=Size;i++){
for(j=1;j<=Size;j++) printf("%f ",Matrix_inverse[i][j]);
printf("\n");
}
printf("Matrix Inverse Low Precision\n");
for(i=1;i<=Size;i++){
for(j=1;j<=Size;j++) printf("%2d ",(int)(10*Matrix_inverse[i][j]));
printf("\n");
}
*/
printf("Matrix Inverse Column\n");
for(i=1;i<=Size;i++){
for(j=1;j<=Size;j++)
printf("%d %d %f\n",i,j,Matrix_inverse[i][j]);
printf("\n");
}

/* check matrix*inverse */
{ float Sum;
for(i=1;i<=Size;i++)
for(j=1;j<=Size;j++) {
Sum=0;
for(k=1;k<=Size;k++) Sum += InputMatrix[i-1][k-1]*Matrix_inverse[k][j];
printf("Sum %d %d = %f\n",i,j,Sum);
}
}

#endif


/* derek: the lisp version, write the output */

 sprintf(Name,"%s.output",argv[6]);
 if((fn=fopen(Name,"w"))==NULL) {
     fprintf(stderr,"Can't open %s\n",Name);
     exit(1);
 }

 fprintf (fn, "(\n");

 fprintf (fn, "(coordss ");
 for(i=1;i<=Size;i++) {
     fprintf (fn, "(");
     for(j=1;j<=OutputDim;j++)
	 fprintf(fn, "%f ", u[i][j]*sqrt(w[j]));
     fprintf (fn, ")");
 }
 fprintf (fn, ")\n");

 fprintf (fn, "(eigenvalues ");
 for(i=1;i<=Size;i++)
     fprintf(fn, "%f ", w[i]);
 fprintf (fn, ")\n");

 fprintf (fn, "(cumulative-variance ");
 Sum=0.;
 for(i=1;i<=Size;i++)
     Sum += w[i];
 for(i=1;i<=Size;i++) {
     Cumulative=0.;
     for(j=1;j<=i;j++)
	 Cumulative += w[j];
     fprintf(fn, "%f ", Cumulative/Sum);
 }
 fprintf (fn, ")\n");

 fprintf (fn, "(cumulative-sd ");
 Sum=0.;
 for(i=1;i<=Size;i++)
     Sum += sqrt(w[i]);
 for(i=1;i<=Size;i++) {
     Cumulative=0.;
     for(j=1;j<=i;j++)
	 Cumulative += sqrt(w[j]);
     fprintf(fn, "%f ", Cumulative/Sum);
 }
 fprintf (fn, ")\n");

 fprintf (fn, ")\n");
 fflush(fn);

}


ReadMatrix(fn)
FILE *fn;
{
char *p,Line[100024],TmpLine[100024];
int col=0,row=0,i,j,last_printable_char;

   while(!feof(fn)) {
      fgets(Line,"%s",fn);
      if(DataHeader) fgets(Line,"%s",fn);
      if(feof(fn)) break;
      last_printable_char=strlen(Line)-2; /* newline and NULL at end */
      if(Line[last_printable_char]==' ') {
         for(i=0;i<last_printable_char;i++) TmpLine[i]=Line[i]; TmpLine[i]='\0';
         for(i=0;i<last_printable_char+1;i++) Line[i]=TmpLine[i];
         }


      col=0;
      p=strtok(Line," ");
      InputMatrix[row][col]=atof(p);col++;
      do {
         p=strtok('\0'," ");
         if(p) {InputMatrix[row][col]=atof(p);col++;}
      } while (p);
      row++;
   }

   if(SquareIt) {
      fprintf(stderr,"Squaring input!\n");
      for(i=0;i<row;i++) {
	 for(j=0;j<col;j++)
	 InputMatrix[i][j] = InputMatrix[i][j]*InputMatrix[i][j];
	 }
      }

/*
printf("DEBUG: InputMatrix, row=%d col=%d\n",row,col);
for(i=0;i<row;i++) {
for(j=0;j<col;j++) printf("%f ",InputMatrix[i][j]);
printf("\n");
}
*/

   /* fprintf(stderr,"read matrix\n");  derek, not for lisp version */


#ifdef KLUDGE1
   for(i=0;i<row;i++) InputMatrix[i][i]++;
#endif

#ifdef KLUDGE2
   for(i=0;i<row;i++) for(j=0;j<row;j++) InputMatrix[i][j]++;
#endif

}
sort(Size)
int Size;
{
/* because of the combination  u[i][j]*sqrt(w[j])) we sort on j */
   int i,j,k;
   float temp_u[MAXCOLS];
   float temp_r;

   for(i=1; i<Size; ++i)
      for(j=i+1; j<=Size; ++j)
          if(w[i]<w[j]) {
	    /* store row i in temp_u */
	    /*
            for(k=1;k<=Size;k++) temp_u[k]=u[i][k];
	    */
            for(k=1;k<=Size;k++) temp_u[k]=u[k][i];
	    /* store value i in temp_r */
            temp_r=w[i];
	    /* replace row i with row j */
	    /*
            for(k=1;k<=Size;k++) u[i][k]=u[j][k];
	    */
            for(k=1;k<=Size;k++) u[k][i]=u[k][j];
	    /* replace value i with value j */
            w[i]=w[j];
	    /* replace row j with row i */
	    /*
            for(k=1;k<=Size;k++) u[j][k]=temp_u[k];
	    */
            for(k=1;k<=Size;k++) u[k][j]=temp_u[k];
	    /* replace value j with value i */
            w[j]=temp_r;
         }

}
