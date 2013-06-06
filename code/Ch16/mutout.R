#include <omp.h>
#include <R.h> 

int tot;  // grand total of matches, over all threads

// processes row pairs (i,i+1), (i,i+2), ...
int procpairs(int i, int *m, int n)
{  int j,k,sum=0;
   for (j = i+1; j < n; j++) {
      for (k = 0; k < n; k++) 
         // find m[i][k]*m[j][k] but remember R uses col-major order
         sum += m[n*k+i] * m[n*k+j];
   }
   return sum;
}

void mutlinks(int *m, int *n, double *mlmean)
{  int nval = *n;
   tot = 0;
   #pragma omp parallel
   {  int i,mysum=0,
          me = omp_get_thread_num(),
          nth = omp_get_num_threads(); 
      // in checking all (i,j) pairs, partition the work according to i;
      // to get good load balance, this thread me will handle all i that equal
      // me mod nth
      for (i = me; i < nval; i += nth) {
         mysum += procpairs(i,m,nval);
      }
      #pragma omp atomic
      tot += mysum;
   }
   int divisor = nval * (nval-1) / 2;
   *mlmean = ((float) tot)/divisor;
}

