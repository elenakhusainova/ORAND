/*-----------------------------------------------------------------------
The code below is part of Christian Borgelt's fim4r package
-----------------------------------------------------------------------*/


#include <stdio.h>
#include <stdlib.h>
#include <float.h>
#include <time.h>
#include <assert.h>
#include <stdarg.h>
#include <string.h>
#include <limits.h>
#include <math.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include "R.h"
#include "Rinternals.h"

/*----------------------------------------------------------------------
Type Definitions
----------------------------------------------------------------------*/
typedef void   OBJFN  (void *obj);
typedef size_t SIZEFN (const void *obj);
typedef int    CMPFN  (const void *p1, const void *p2, void *data);
typedef double RANDFN (void);
#define diff_t        ptrdiff_t /* signed variant of size_t */
#define OBJ_MAXSIZE   256       /* maximum size of objects */

typedef int int_CMPFN (int    i1, int    i2, void *data);
typedef int lng_CMPFN (long   i1, long   i2, void *data);
typedef int dif_CMPFN (diff_t i1, diff_t i2, void *data);

void   int_reverse  (int    *array, size_t n);
void   int_qsort    (int    *array, size_t n, int dir);
void   int_heapsort (int    *array, size_t n, int dir);
size_t int_unique   (int    *array, size_t n);
diff_t int_bsearch  (int    key, const int    *array, size_t n);
size_t int_bisect   (int    key, const int    *array, size_t n);

size_t siz_bisect   (size_t key, const size_t *array, size_t n);
void   dbl_reverse  (double *array, size_t n);
void   dbl_qsort    (double *array, size_t n, int dir);
size_t dbl_bisect   (double key, const double *array, size_t n);

void   ptr_reverse  (void *array, size_t n);
void   ptr_qsort    (void *array, size_t n, int dir,
                     CMPFN *cmp, void *data);
void   ptr_heapsort (void *array, size_t n, int dir,
                     CMPFN *cmp, void *data);
int    ptr_mrgsort  (void *array, size_t n, int dir,
                     CMPFN *cmp, void *data, void *buf);

void   i2i_qsort    (int *index, size_t n, int dir,
                     const int    *array);

#define BUFSIZE     1024        /* size of fixed buffer for moving */
#define OBJSIZE     ((OBJ_MAXSIZE +sizeof(size_t)-1)/sizeof(size_t))
#define TH_INSERT   16          /* threshold for insertion sort */


#define REVERSE(name, type)                                                  \
void name##_reverse (type *array, size_t n)                                  \
{                               /* --- reverse a number array */             \
type *end = array +n;         /* end of the array to reverse */              \
type t;                       /* exchange buffer */                          \
                                                                             \
while (--end > array) {       /* reverse the order of the elems. */          \
t = *end; *end = *array; *array++ = t; }                                     \
}  /* reverse */

/*--------------------------------------------------------------------*/

REVERSE(sht, short)
  REVERSE(int, int)
  REVERSE(lng, long)
  REVERSE(dif, diff_t)
  REVERSE(siz, size_t)
  REVERSE(flt, float)
  REVERSE(dbl, double)
  
  /*--------------------------------------------------------------------*/
  
#define QSORT(name,type)                                                       \
  static void name##_qrec (type *a, size_t n)                                  \
  {                               /* --- recursive part of sort */             \
  type   *l, *r;                /* pointers to exchange positions */           \
  type   x, t;                  /* pivot element and exchange buffer */        \
  size_t m;                     /* number of elements in sections */           \
                                                                               \
  do {                          /* sections sort loop */                       \
  l = a; r = l +n -1;         /* start at left and right boundary */           \
  if (*l > *r) { t = *l; *l = *r; *r = t; }                                    \
  x = a[n/2];                 /* get the middle element as pivot */            \
  if      (x < *l) x = *l;    /* compute median of three */                    \
  else if (x > *r) x = *r;    /* to find a better pivot */                     \
  while (1) {                 /* split and exchange loop */                    \
  while (*++l < x);         /* skip smaller elems. on the left */              \
  while (*--r > x);         /* skip greater elems. on the right */             \
  if (l >= r) {             /* if at most one element left, */                 \
  if (l <= r) { l++; r--; } break; }       /* abort the loop */                \
  t = *l; *l = *r; *r = t;  /* otherwise exchange elements */                  \
  }                                                                            \
  m = n -(size_t)(l-a);       /* compute the number of elements */             \
  n = 1 +(size_t)(r-a);       /* right and left of the split */                \
  if (n > m) {                /* if right section is smaller, */               \
  if (m >= TH_INSERT)       /* but larger than the threshold, */               \
  name##_qrec(l, m); }    /* sort it by an recursive call */                   \
  else {                      /* if the left section is smaller, */            \
  if (n >= TH_INSERT)       /* but larger than the threshold, */               \
  name##_qrec(a, n);      /* sort it by an recursive call, */                  \
  a = l; n = m;             /* then switch to the right section */             \
  }                           /* keeping its size m in variable n */           \
  } while (n >= TH_INSERT);     /* while greater than threshold */             \
  }  /* qrec() */                                                              \
                                                                               \
  /*------------------------------------------------------------------*/       \
                                                                               \
  void name##_qsort (type *array, size_t n, int dir)                           \
  {                               /* --- sort a number array */                \
  size_t i, k;                  /* loop variable, first section */             \
  type   *l, *r;                /* to traverse the array */                    \
  type   t;                     /* exchange buffer */                          \
                                                                               \
  assert(array);                /* check the function arguments */             \
  if (n < 2) return;            /* do not sort less than two elems. */         \
  if (n < TH_INSERT)            /* if less elements than threshold */          \
  k = n;                      /* for insertion sort, note the */               \
  else {                        /* number of elements, otherwise */            \
  name##_qrec(array, n);      /* call the recursive sort function */           \
  k = TH_INSERT -1;           /* and get the number of elements */             \
  }                             /* in the first array section */               \
  for (l = r = array; --k > 0;) /* find position of smallest element */        \
  if (*++r < *l) l = r;       /* within the first k elements */                \
  r = array;                    /* swap the smallest element */                \
  t = *l; *l = *r; *r = t;      /* to the front as a sentinel */               \
  for (i = n; --i > 0; ) {      /* standard insertion sort */                  \
  t = *++r;                   /* note the number to insert */                  \
  for (l = r; *--l > t; )     /* shift right all numbers that are */           \
  l[1] = *l;                /* greater than the one to insert */               \
  l[1] = t;                   /* and store the number to insert */             \
  }                             /* in the place thus found */                  \
  if (dir < 0)                  /* if descending order requested, */           \
  name##_reverse(array, n);   /* reverse the element order */                  \
  }  /* qsort() */

/*--------------------------------------------------------------------*/

QSORT(sht, short)
    QSORT(int, int)
    QSORT(lng, long)
    QSORT(dif, diff_t)
    QSORT(siz, size_t)
    QSORT(flt, float)
    QSORT(dbl, double)
    
    /*--------------------------------------------------------------------*/
    
#define HEAPSORT(name,type)                                                      \
    static void name##_sift (type *array, size_t l, size_t r)                    \
    {                               /* --- let element sift down in heap */      \
    size_t i;                     /* index of first successor in heap */         \
    type   t;                     /* buffer for an array element */              \
                                                                                 \
    t = array[l];                 /* note the sift element */                    \
    i = l +l +1;                  /* compute index of first successor */         \
    do {                          /* sift loop */                                \
    if ((i < r) && (array[i] < array[i+1]))                                      \
      i++;                      /* if second successor is greater */             \
    if (t >= array[i])          /* if the successor is greater */                \
    break;                    /* than the sift element, */                       \
    array[l] = array[i];        /* let the successor ascend in heap */           \
    l = i; i += i +1;           /* compute index of first successor */           \
    } while (i <= r);             /* while still within heap */                  \
    array[l] = t;                 /* store the sift element */                   \
    }  /* sift() */                                                              \
                                                                                 \
    /*------------------------------------------------------------------*/       \
                                                                                 \
    void name##_heapsort (type *array, size_t n, int dir)                        \
    {                               /* --- heap sort for number arrays */        \
    size_t l, r;                  /* boundaries of heap section */               \
    type   t;                     /* exchange buffer */                          \
                                                                                 \
    assert(array);                /* check the function arguments */             \
    if (n < 2) return;            /* do not sort less than two elems. */         \
    l = n /2;                     /* at start, only the second half */           \
    r = n -1;                     /* of the array has heap structure */          \
    while (l > 0)                 /* while the heap is not complete, */          \
    name##_sift(array, --l, r); /* extend it by one element */                   \
    while (1) {                   /* heap reduction loop */                      \
    t = array[0];               /* swap the greatest element */                  \
    array[0] = array[r];        /* to the end of the array */                    \
    array[r] = t;                                                                \
    if (--r <= 0) break;        /* if the heap is empty, abort */                \
    name##_sift(array, 0, r);   /* let swapped element sift down */              \
    }                                                                            \
    if (dir < 0)                  /* if descending order requested, */           \
    name##_reverse(array, n);   /* reverse the element order */                  \
    }  /* heapsort() */
  HEAPSORT(int, int)
      
      /*--------------------------------------------------------------------*/
      
#define UNIQUE(name,type)                                                          \
      size_t name##_unique (type *array, size_t n)                                 \
      {                               /* --- remove duplicate elements */          \
      type *s, *d;                  /* to traverse the array */                    \
                                                                                   \
      assert(array);                /* check the function arguments */             \
      if (n <= 1) return n;         /* check for 0 or 1 element */                 \
      for (d = s = array; --n > 0;) /* traverse the (sorted) array and */          \
      if (*++s != *d) *++d = *s;  /* collect the unique elements */                \
      return (size_t)(++d -array);  /* return new number of elements */            \
      }  /* unique() */
    
    UNIQUE(int, int)
        
        
        /*--------------------------------------------------------------------*/
        
#define BSEARCH(name,type)                                                           \
        diff_t name##_bsearch (type key, const type *array, size_t n)                \
        {                               /* --- do a binary search */                 \
        size_t l, r, m;               /* array indices */                            \
        type   t;                     /* array element */                            \
                                                                                     \
        assert(array);                /* check the function arguments */             \
        for (l = 0, r = n; l < r; ) { /* while search range is not empty */          \
        t = array[m = (l+r)/2];     /* compare the given key */                      \
        if      (key > t) l = m+1;  /* to the middle element and */                  \
        else if (key < t) r = m;    /* adapt the search range */                     \
        else return (diff_t)m;      /* according to the result */                    \
        }                             /* if match found, return index */             \
        return (diff_t)-1;            /* return 'not found' */                       \
        }  /* bsearch() */
      
      BSEARCH(int, int)
          /*--------------------------------------------------------------------*/
          
#define BISECT(name,type)                                                              \
          size_t name##_bisect (type key, const type *array, size_t n)                 \
          {                               /* --- do a bisection search */              \
          size_t l, r, m;               /* array indices */                            \
          type   t;                     /* array element */                            \
                                                                                       \
          assert(array);                /* check the function arguments */             \
          for (l = 0, r = n; l < r; ) { /* while search range is not empty */          \
          t = array[m = (l+r) /2];    /* compare the given key */                      \
          if      (key > t) l = m+1;  /* to the middle element and */                  \
          else if (key < t) r = m;    /* adapt the search range */                     \
          else return m;              /* according to the result */                    \
          }                             /* if match found, return index */             \
          return l;                     /* return the insertion position */            \
          }  /* bisect() */
        
        /*--------------------------------------------------------------------*/
        
        BISECT(sht, short)
            BISECT(int, int)
            BISECT(lng, long)
            BISECT(dif, diff_t)
            BISECT(siz, size_t)
            BISECT(flt, float)
            BISECT(dbl, double)
            
            /*----------------------------------------------------------------------
            Functions for Pointer Arrays
            ----------------------------------------------------------------------*/
            
            
            void ptr_reverse (void *array, size_t n)
            {                               /* --- reverse a pointer array */
            void **a = (void**)array;     /* array to reverse */
            void **e = a +n;              /* end of array to reverse */
            void *t;                      /* exchange buffer */
            
            assert(array);                /* check the function arguments */
            while (--e > a) {             /* reverse the order of the elements */
            t = *e; *e = *a; *a++ = t; }
            }  /* ptr_reverse() */
            
            /*--------------------------------------------------------------------*/
            
            static void ptr_qrec (void **a, size_t n, CMPFN *cmp, void *data)
            {                               /* --- recursive part of quicksort */
            void   **l, **r;              /* pointers to exchange positions */
            void   *x, *t;                /* pivot element and exchange buffer */
            size_t m;                     /* number of elements in 2nd section */
            
            do {                          /* sections sort loop */
            l = a; r = l +n -1;         /* start at left and right boundary */
            if (cmp(*l, *r, data) > 0){ /* bring the first and last */
            t = *l; *l = *r; *r = t;} /* element into proper order */
            x = a[n/2];                 /* get the middle element as pivot */
            if      (cmp(x, *l, data) < 0) x = *l;  /* try to find a */
            else if (cmp(x, *r, data) > 0) x = *r;  /* better pivot */
            while (1) {                 /* split and exchange loop */
            while (cmp(*++l, x, data) < 0)      /* skip left  elements that */
            ;                       /* are smaller than the pivot element */
            while (cmp(*--r, x, data) > 0)      /* skip right elements that */
            ;                       /* are greater than the pivot element */
            if (l >= r) {             /* if at most one element left, */
            if (l <= r) { l++; r--; } break; }    /* abort the loop */
            t = *l; *l = *r; *r = t;  /* otherwise exchange elements */
            }
            m = n -(size_t)(l-a);       /* compute the number of elements */
            n = 1 +(size_t)(r-a);       /* right and left of the split */
            if (n > m) {                /* if right section is smaller, */
            if (m >= TH_INSERT)       /* but larger than the threshold, */
            ptr_qrec(l,m,cmp,data);}/* sort it by a recursive call, */
            else {                      /* if the left section is smaller, */
            if (n >= TH_INSERT)       /* but larger than the threshold, */
            ptr_qrec(a,n,cmp,data); /* sort it by a recursive call, */
            a = l; n = m;             /* then switch to the right section */
            }                           /* keeping its size m in variable n */
            } while (n >= TH_INSERT);     /* while greater than threshold */
            }  /* ptr_qrec() */
            
            /*--------------------------------------------------------------------*/
            
            void ptr_qsort (void *array, size_t n, int dir, CMPFN *cmp, void *data)
            {                               /* --- quicksort for pointer arrays */
            size_t i, k;                  /* loop variable, first section */
            void   **l, **r;              /* to traverse the array */
            void   *t;                    /* exchange buffer */
            
            assert(array && cmp);         /* check the function arguments */
            if (n < 2) return;            /* do not sort less than two elements */
            if (n < TH_INSERT)            /* if fewer elements than threshold */
            k = n;                      /* for insertion sort, note the */
            else {                        /* number of elements, otherwise */
            ptr_qrec((void**)array, n, cmp, data);
              k = TH_INSERT -1;           /* call the recursive function and */
            }                             /* get size of first array section */
            for (l = r = (void**)array; --k > 0; )
              if (cmp(*++r, *l, data) < 0)
                l = r;                    /* find smallest of first k elements */
            r = (void**)array;            /* swap the smallest element */
            t = *l; *l = *r; *r = t;      /* to the front as a sentinel */
            for (i = n; --i > 0; ) {      /* standard insertion sort */
            t = *++r;                   /* note the element to insert */
            for (l = r; cmp(*--l, t, data) > 0; ) /* shift right elements */
            l[1] = *l;                /* that are greater than the one */
            l[1] = t;                   /* to insert and store this element */
            }                             /* in the place thus found */
            if (dir < 0)                  /* if descending order requested, */
            ptr_reverse(array, n);      /* reverse the element order */
            }  /* ptr_qsort() */
            
            /*--------------------------------------------------------------------*/
            
            static void ptr_sift (void **array, size_t l, size_t r,
                                  CMPFN *cmp, void *data)
            {                               /* --- let element sift down in heap */
            size_t i;                     /* index of first successor in heap */
            void   *t;                    /* buffer for an array element */
            
            t = array[l];                 /* note the sift element */
            i = l +l +1;                  /* compute index of first successor */
            do {                          /* sift loop */
            if ((i < r)                 /* if second successor is greater */
            &&  (cmp(array[i], array[i+1], data) < 0))
              i++;                      /* go to the second successor */
            if (cmp(t, array[i], data) >= 0) /* if the successor is greater */
            break;                         /* than the sift element, */
            array[l] = array[i];        /* let the successor ascend in heap */
            l = i; i += i +1;           /* compute index of first successor */
            } while (i <= r);             /* while still within heap */
            array[l] = t;                 /* store the sift element */
            }  /* ptr_sift() */
            
            /*--------------------------------------------------------------------*/
            
            void ptr_heapsort (void *array, size_t n, int dir,
                               CMPFN *cmp, void *data)
            {                               /* --- heap sort for pointer arrays */
            size_t l, r;                  /* boundaries of heap section */
            void   *t;                    /* exchange buffer */
            void   **a = (void**)array;   /* typed array */
            
            assert(array && cmp);         /* check the function arguments */
            if (n < 2) return;            /* do not sort less than two elements */
            l = n /2;                     /* at start, only the second half */
            r = n -1;                     /* of the array has heap structure */
            while (l > 0)                 /* while the heap is not complete, */
            ptr_sift(a,--l,r,cmp,data); /* extend it by one element */
            while (1) {                   /* heap reduction loop */
            t = a[0]; a[0] = a[r];      /* swap the greatest element */
            a[r] = t;                   /* to the end of the array */
            if (--r <= 0) break;        /* if the heap is empty, abort */
            ptr_sift(a,0,r,cmp,data);   /* let the element that has been */
            }                             /* swapped to front sift down */
            if (dir < 0)                  /* if descending order requested, */
            ptr_reverse(array, n);      /* reverse the element order */
            }  /* ptr_heapsort() */
            
            /*--------------------------------------------------------------------*/
            
            static void mrgsort (void **array, void **buf, size_t n,
                                 CMPFN *cmp, void *data)
            {                               /* --- merge sort for pointer arrays */
            size_t k, a, b;               /* numbers of objects in sections */
            void   **sa, **sb, **ea, **eb;/* starts and ends of sorted sections */
            void   **d, *t;               /* merge destination, exchange buffer */
            
            assert(array && buf && cmp);  /* check the function arguments */
            if (n <= 8) {                 /* if only few elements to sort */
            for (sa = array; --n > 0;){ /* insertion sort loop */
            t = *(d = ++sa);          /* note the element to insert */
            while ((--d >= array)     /* while not at the array start, */
            &&     (cmp(*d, t, data) > 0))     /* shift right elements */
            d[1] = *d;              /* that are greater than the one */
            d[1] = t;                 /* to insert and store the element */
            } return;                   /* to insert in the place thus found */
            }                             /* aftwards sorting is done, so abort */
            /* Using insertion sort for less than eight elements is not only */
            /* slightly faster, but also ensures that all subsections sorted */
            /* recursively in the code below contain at least two elements.  */
            
            k = n/2; d = buf;             /* sort two subsections recursively */
            mrgsort(sa = array,   d,   a = k/2, cmp, data);
            mrgsort(sb = sa+a,    d+a, b = k-a, cmp, data);
            for (ea = sb, eb = sb+b; 1;){ /* traverse the sorted sections */
            if (cmp(*sa, *sb, data) <= 0)
            { *d++ = *sa++; if (sa >= ea) break; }
            else { *d++ = *sb++; if (sb >= eb) break; }
            }                             /* copy smaller element to dest. */
            while (sa < ea) *d++ = *sa++; /* copy remaining elements */
            while (sb < eb) *d++ = *sb++; /* from source to destination */
            
            n -= k; d = buf+k;            /* sort two subsections recursively */
            mrgsort(sa = array+k, d,   a = n/2, cmp, data);
            mrgsort(sb = sa+a,    d+a, b = n-a, cmp, data);
            for (ea = sb, eb = sb+b; 1;){ /* traverse the sorted sections */
            if (cmp(*sa, *sb, data) <= 0)
            { *d++ = *sa++; if (sa >= ea) break; }
            else { *d++ = *sb++; if (sb >= eb) break; }
            }                             /* copy smaller element to dest. */
            while (sa < ea) *d++ = *sa++; /* copy remaining elements */
            while (sb < eb) *d++ = *sb++; /* from source to destination */
            
            sa = buf; sb = sa+k; d = array;
            for (ea = sb, eb = sb+n; 1;){ /* traverse the sorted sections */
            if (cmp(*sa, *sb, data) <= 0)
            { *d++ = *sa++; if (sa >= ea) break; }
            else { *d++ = *sb++; if (sb >= eb) break; }
            }                             /* copy smaller element to dest. */
            while (sa < ea) *d++ = *sa++; /* copy remaining elements */
            while (sb < eb) *d++ = *sb++; /* from source to destination */
            }  /* mrgsort() */
            
            /*--------------------------------------------------------------------*/
            
            int ptr_mrgsort (void *array, size_t n, int dir,
                             CMPFN *cmp, void *data, void *buf)
            {                               /* --- merge sort for pointer arrays */
            void **b;                     /* (allocated) buffer */
            
            assert(array && cmp);         /* check the function arguments */
            if (n < 2) return 0;          /* do not sort less than two objects */
            if (!(b = (void**)buf) && !(b = (void**)malloc(n *sizeof(void*))))
              return -1;                  /* allocate a buffer if not given */
            mrgsort(array, buf, n, cmp, data);
            if (!buf) free(b);            /* sort the array recursively */
            if (dir < 0)                  /* if descending order requested, */
            ptr_reverse(array, n);      /* reverse the element order */
            return 0;                     /* return 'ok' */
            }  /* ptr_mrgsort() */
            
            /* This implementation of merge sort is stable, that is, it does not  */
            /* change the relative order of elements that are considered equal by */
            /* the comparison function. Thus it maintains the order of a previous */
            /* sort with another comparison function as long as the order imposed */
            /* by the current comparison function does not override this order.   */
            
            /*----------------------------------------------------------------------
            Functions for Index Arrays
          ----------------------------------------------------------------------*/
            
#define IDX_QSORT(name,tin,tidx,type)                                                    \
            static void name##_qrec (tidx *index, size_t n, const type *array)           \
            {                               /* --- recursive part of sort */             \
            tidx   *l, *r;                /* pointers to exchange positions */           \
            tidx   t;                     /* pivot element and exchange buffer */        \
            size_t m;                     /* number of elements in sections */           \
            type   p, a, z;               /* buffers for array elements */               \
                                                                                         \
            do {                          /* sections sort loop */                       \
            l = index; r = l +n -1;     /* start at left and right boundary */           \
            a = array[*l];              /* get the first and last elements */            \
            z = array[*r];              /* and bring them into right order */            \
            if (a > z) { t = *l; *l = *r; *r = t; }                                      \
            t = index[n /2];            /* get the middle element as pivot */            \
            p = array[t];               /* and array element referred to */              \
            if      (p < a) { p = a; t = *l; }  /* compute median of three */            \
            else if (p > z) { p = z; t = *r; }  /* to find a better pivot */             \
            while (1) {                 /* split and exchange loop */                    \
            while (array[*++l] < p);  /* skip smaller elems. on the left */              \
            while (array[*--r] > p);  /* skip greater elems. on the right */             \
            if (l >= r) {             /* if at most one element left, */                 \
            if (l <= r) { l++; r--; } break; }    /* abort the loop */                   \
            t = *l; *l = *r; *r = t;  /* otherwise exchange elements */                  \
            }                                                                            \
            m = n -(size_t)(l-index);   /* compute the number of elements */             \
            n = 1 +(size_t)(r-index);   /* right and left of the split */                \
            if (n > m) {                /* if right section is smaller, */               \
            if (m >= TH_INSERT)       /* but larger than the threshold, */               \
            name##_qrec(l,     m, array); }   /* sort it recursively, */                 \
            else {                      /* if the left section is smaller, */            \
            if (n >= TH_INSERT)       /* but larger than the threshold, */               \
            name##_qrec(index, n, array);     /* sort it recursively, */                 \
            index = l; n = m;         /* then switch to the right section */             \
            }                           /* keeping its size m in variable n */           \
            } while (n >= TH_INSERT);     /* while greater than threshold */             \
            }  /* qrec() */                                                              \
                                                                                         \
            /*------------------------------------------------------------------*/       \
                                                                                         \
            void name##_qsort (tidx *index, size_t n, int dir, const type *array)        \
            {                               /* --- sort an index array */                \
            size_t i, k;                  /* loop variable, first section */             \
            tidx   *l, *r;                /* to traverse the array */                    \
            tidx   x;                     /* exchange buffer */                          \
            type   t;                     /* buffer for element referred to */           \
                                                                                         \
            assert(index && array);       /* check function arguments */                 \
            if (n < 2) return;            /* do not sort less than two elems. */         \
            if (n < TH_INSERT)            /* if less elements than threshold */          \
            k = n;                      /* for insertion sort, note the */               \
            else {                        /* number of elements, otherwise */            \
            name##_qrec(index, n, array);     /* call recursive function */              \
            k = TH_INSERT -1;           /* and get the number of elements */             \
            }                             /* in the first array section */               \
            for (l = r = index; --k > 0;) /* find the position */                        \
            if (array[*++r] < array[*l])/* of the smallest element */                    \
            l = r;                    /* within the first k elements */                  \
            r = index;                    /* swap the smallest element */                \
            x = *l; *l = *r; *r = x;      /* to front as a sentinel */                   \
            for (i = n; --i > 0; ) {      /* standard insertion sort */                  \
            t = array[x = *++r];        /* note the number to insert */                  \
            for (l = r; array[*--l] > t; ) /* shift right all that are */                \
            l[1] = *l;                /* greater than the one to insert */               \
            l[1] = x;                   /* and store the number to insert */             \
            }                             /* in the place thus found */                  \
            if (dir < 0)                  /* if descending order requested, */           \
            tin##_reverse(index, n);    /* reverse the element order */                  \
            }  /* qsort() */
          
          /*--------------------------------------------------------------------*/
          
          IDX_QSORT(i2i, int, int,    int)
              IDX_QSORT(i2l, int, int,    long)
              IDX_QSORT(i2x, int, int,    diff_t)
              IDX_QSORT(i2z, int, int,    size_t)
              IDX_QSORT(i2f, int, int,    float)
              IDX_QSORT(i2d, int, int,    double)
              
              IDX_QSORT(l2i, lng, long,   int)
              IDX_QSORT(l2l, lng, long,   long)
              IDX_QSORT(l2x, lng, long,   diff_t)
              IDX_QSORT(l2z, lng, long,   size_t)
              IDX_QSORT(l2f, lng, long,   float)
              IDX_QSORT(l2d, lng, long,   double)
              
              IDX_QSORT(x2i, dif, diff_t, int)
              IDX_QSORT(x2l, dif, diff_t, long)
              IDX_QSORT(x2x, dif, diff_t, diff_t)
              IDX_QSORT(x2z, dif, diff_t, size_t)
              IDX_QSORT(x2f, dif, diff_t, float)
              IDX_QSORT(x2d, dif, diff_t, double)
              /*----------------------------------------------------------------------
              Preprocessor Definitions
              ----------------------------------------------------------------------*/
#define RNG_UNIFORM     0       /* uniform     density function */
#define RNG_RECT        0       /* rectangular density function */
#define RNG_TRIANG      1       /* triangular  density function */
#define RNG_GAUSS       2       /* Gaussian    density function */
#define RNG_NORMAL      2       /* normal      density function */
              
              /*----------------------------------------------------------------------
              Type Definitions
----------------------------------------------------------------------*/
              typedef struct {                /* --- random number generator --- */
              unsigned int state[5];        /* registers for generator state */
              double       b;               /* buffer for Box-Muller transform */
              int          type;            /* density type (e.g. RNG_RECT) */
              double       sigma;           /* std. dev. or width parameter */
              } RNG;                          /* (random number generator) */
              
              typedef double RNGFN (RNG *rng);
              
              /*----------------------------------------------------------------------
              Constants
              ----------------------------------------------------------------------*/
              RNGFN *rng_tab[];        /* table of random number functions */
              
              /*----------------------------------------------------------------------
              Random Number Functions
              ----------------------------------------------------------------------*/
              
              void         rng_delete  (RNG *rng);
              unsigned int rng_uint    (RNG *rng);
              double       rng_dbl     (RNG *rng);
              double       rng_dblx    (RNG *rng);
              int          rng_bit     (RNG *rng);
              double       rng_norm    (RNG *rng);
              
              int          rng_type    (RNG *rng);
              double       rng_sigma   (RNG *rng);
              double       rng_next    (RNG *rng);
              double       rng_uniform (RNG *rng);
              double       rng_rect    (RNG *rng);
              double       rng_triang  (RNG *rng);
              double       rng_gauss   (RNG *rng);
              double       rng_normal  (RNG *rng);
              
              /*----------------------------------------------------------------------
              Preprocessor Definitions
              ----------------------------------------------------------------------*/
#define rng_delete(g)   free(g)
#define rng_type(g)     ((g)->type)
#define rng_sigma(g)    ((g)->sigma)
#define rng_next(g)     (rng_tab[(g)->type](g))
#define rng_uniform(g)  rng_rect(g)
#define rng_gauss(g)    rng_normal(g)
              
#define RNORM       (1.0 /((double)UINT_MAX +1.0))
              
#define INFINITY    (DBL_MAX+DBL_MAX)
#define NAN         (INFINITY-INFINITY)
              
              /*----------------------------------------------------------------------
              Random Number Functions
----------------------------------------------------------------------*/
              
              RNGFN *rng_tab[] = {            /* table of random functions */
              /* RNG_UNIFORM/RNG_RECT  0 */ rng_rect,
              /* RNG_TRIANG            1 */ rng_triang,
              /* RNG_GAUSS/RNG_NORMAL  2 */ rng_normal,
              };
              
              /*----------------------------------------------------------------------
              (Pseudo-)Random Number Generator Functions
              ----------------------------------------------------------------------*/
              
              unsigned int rng_uint (RNG *rng)
              {                               /* --- generate random unsigned int */
              unsigned int t = (rng->state[0] ^ (rng->state[0] >> 7));
                rng->state[0] = rng->state[1]; rng->state[1] = rng->state[2];
                rng->state[2] = rng->state[3]; rng->state[3] = rng->state[4];
                rng->state[4] = (rng->state[4] ^ (rng->state[4] << 6))
                  ^ (t ^ (t << 13));
                return (rng->state[1] +rng->state[1] +1) *rng->state[4];
              }  /* rng_uint() */
              
              /*--------------------------------------------------------------------*/
              
              double rng_dbl (RNG *rng)
              { return rng_uint(rng) *RNORM; }
              
              /*--------------------------------------------------------------------*/
              
              double rng_dblx (RNG *rng)
              {                               /* --- random double with full prec. */
              double x;                     /* buffer for random number */
              do { x = rng_uint(rng) *RNORM +rng_uint(rng) *(RNORM *RNORM);
              } while (x >= 1);             /* ensure that the number is < 1 */
              return x;                     /* return the generated number */
              }  /* rng_dblx() */
              
              /*--------------------------------------------------------------------*/
              
              int rng_bit (RNG *rng)
              { return (rng_uint(rng) > UINT_MAX /2) ? 1 : 0; }
              
              /*--------------------------------------------------------------------*/
              
              double rng_norm (RNG *rng)
              {                               /* --- generate std. normal value */
              double x, y, r;               /* coordinates and radius */
              
              if (!isnan(rng->b)) {         /* if the buffer is full, return it */
              x = rng->b; rng->b = NAN; return x; }
              do {                          /* pick a random point */
              x = 2 *rng_dbl(rng) -1;     /* in the unit square [-1,1]^2 */
              y = 2 *rng_dbl(rng) -1;     /* and check whether it lies */
              r = x*x +y*y;               /* inside the unit circle */
              } while ((r > 1) || (r == 0));
              r = sqrt(-2*log(r)/r);        /* factor for Box-Muller transform */
              rng->b = x *r;                /* save one of the random numbers */
              return y *r;                  /* and return the other */
              }  /* rng_norm() */
              
              /*----------------------------------------------------------------------
              Symmetric Density (Pseudo-)Random Number Generator Functions
              ----------------------------------------------------------------------*/
              
              double rng_rect (RNG *rng)
              { return (rng->sigma > 0) ? (rng_dbl(rng)-0.5) *2*rng->sigma : 0;}
              
              /*--------------------------------------------------------------------*/
              
              double rng_triang (RNG *rng)
              {                               /* --- triangular density */
              double y;                     /* sample from uniform density */
              if (rng->sigma <= 0) return 0;/* return zero if sigma vanishes */
              y = rng_dbl(rng);             /* transform from uniform density */
              return rng->sigma *((y < 0.5) ? sqrt(2*y)-1.0 : 1.0-sqrt(2*(1.0-y)));
              }  /* rng_triang() */
              
              /*--------------------------------------------------------------------*/
              
              double rng_normal (RNG *rng)
              { return (rng->sigma > 0) ? rng_norm(rng) *rng->sigma : 0; }
              
              
              
              /*----------------------------------------------------------------------
              Preprocessor Definitions
              ----------------------------------------------------------------------*/
#define IDENT       int         /* identifier type for map */
#define SIZE_FMT  "zu"        /* printf format code for size_t */
              
              /*--------------------------------------------------------------------*/
              
#define EXISTS    ((void*)-1)   /* symbol exists already */
#define IDMAP     SYMTAB        /* id maps are special symbol tables */
              
              /* --- abbreviations for standard function sets --- */
#define ST_STRFN  st_strhash, st_strcmp, NULL
#define ST_INTFN  st_inthash, st_intcmp, NULL
#define ST_LNGFN  st_lnghash, st_lngcmp, NULL
#define ST_SIZFN  st_sizhash, st_sizcmp, NULL
#define ST_DIFFN  st_difhash, st_difcmp, NULL
#define ST_PTRFN  st_ptrhash, st_ptrcmp, NULL
              
              /*----------------------------------------------------------------------
              Type Definitions
----------------------------------------------------------------------*/
              typedef size_t HASHFN (const void *key, int type);
              
              typedef struct ste {            /* --- symbol table element --- */
              struct ste *succ;             /* successor in hash bin */
              void       *key;              /* symbol name/key */
              int        type;              /* symbol type */
              size_t     level;             /* visibility level */
              } STE;                          /* (symbol table element) */
              
              typedef struct {                /* --- symbol table --- */
              size_t     cnt;               /* current number of symbols */
              size_t     level;             /* current visibility level */
              size_t     size;              /* current hash table size */
              size_t     max;               /* maximal hash table size */
              HASHFN     *hashfn;           /* hash function */
              CMPFN      *cmpfn;            /* comparison function */
              void       *data;             /* comparison data */
              OBJFN      *delfn;            /* symbol deletion function */
              STE        **bins;            /* array of hash bins */
              size_t     idsize;            /* size of identifier array */
              IDENT      **ids;             /* identifier array */
              } SYMTAB;                       /* (symbol table) */
              
              /*----------------------------------------------------------------------
              Name/Key Functions
              ----------------------------------------------------------------------*/
              int         st_strcmp  (const void *a, const void *b, void *d);
              size_t      st_strhash (const void *s, int type);
              
              int         st_intcmp  (const void *a, const void *b, void *d);
              size_t      st_inthash (const void *i, int type);
              
              int         st_lngcmp  (const void *a, const void *b, void *d);
              size_t      st_lnghash (const void *i, int type);
              
              int         st_difcmp  (const void *a, const void *b, void *d);
              size_t      st_difhash (const void *i, int type);
              
              int         st_ptrcmp  (const void *a, const void *b, void *d);
              size_t      st_ptrhash (const void *p, int type);
              
              /*----------------------------------------------------------------------
              Symbol Table Functions
              ----------------------------------------------------------------------*/
              SYMTAB*     st_create  (size_t init, size_t max, HASHFN hashfn,
                                      CMPFN cmpfn, void *data, OBJFN delfn);
              void        st_delete  (SYMTAB *tab);
              void*       st_insert  (SYMTAB *tab, const void *key, int type,
                                      size_t keysize, size_t datasize);
              int         st_remove  (SYMTAB *tab, const void *key, int type);
              void*       st_lookup  (SYMTAB *tab, const void *key, int type);
              void        st_begblk  (SYMTAB *tab);
              size_t      st_symcnt  (const SYMTAB *tab);
              const char* st_name    (const void *data);
              const void* st_key     (const void *data);
              int         st_type    (const void *data);
              
              /*----------------------------------------------------------------------
              Identifier Map Functions
              ----------------------------------------------------------------------*/
              
              IDMAP*      idm_create (size_t init, size_t max, HASHFN hashfn,
                                      CMPFN cmpfn, void *data, OBJFN delfn);
              void        idm_delete (IDMAP* idm);
              void*       idm_add    (IDMAP* idm, const void *key,
                                      size_t keysize, size_t datasize);
              void*       idm_byname (IDMAP* idm, const char *name);
              void*       idm_bykey  (IDMAP* idm, const void *key);
              void*       idm_byid   (IDMAP* idm, IDENT id);
              IDENT       idm_getid  (IDMAP* idm, const void *name);
              const char* idm_name   (const void *data);
              const void* idm_key    (const void *data);
              IDENT       idm_cnt    (const IDMAP *idm);
              void        idm_sort   (IDMAP *idm, CMPFN cmpfn, void *data,
                                      IDENT *map, int dir);
              void        idm_trunc  (IDMAP *idm, size_t n);
              /*----------------------------------------------------------------------
              Preprocessor Definitions
              ----------------------------------------------------------------------*/
#define st_begblk(t)      ((t)->level++)
#define st_symcnt(t)      ((t)->cnt)
#define st_name(d)        ((const char*)((STE*)(d)-1)->key)
#define st_key(d)         ((const void*)((STE*)(d)-1)->key)
#define st_type(d)        (((STE*)(d)-1)->type)
              
              /*--------------------------------------------------------------------*/
#define idm_delete(m)     st_delete(m)
#define idm_add(m,n,k,s)  st_insert(m,n,0,k,s)
#define idm_byname(m,n)   st_lookup(m,n,0)
#define idm_bykey(m,k)    st_lookup(m,k,0)
#define idm_byid(m,i)     ((void*)(m)->ids[i])
#define idm_name(d)       st_name(d)
#define idm_key(d)        st_key(d)
#define idm_cnt(m)        ((IDENT)st_symcnt(m))
              
              /*----------------------------------------------------------------------
              Preprocessor Definitions
----------------------------------------------------------------------*/
#define DFLT_INIT    32767      /* default initial hash table size */
#define DFLT_MAX   4194303      /* default maximal hash table size */
#define BLKSIZE       4096      /* block size for identifier array */
#define ALIGN            4      /* alignment to addresses that are */
              
              
              /*----------------------------------------------------------------------
              Name/Key Functions (string)
----------------------------------------------------------------------*/
              
              int st_strcmp (const void *a, const void *b, void *data)
              {                               /* --- string comparison function */
              const char *s = a, *t = b;    /* to traverse the strings */
              while (*s)                    /* while more characters to compare */
              if (*s++ != *t++) return -1;/* if characters differ, abort */
              return (*t) ? -1 : 0;         /* at end of 1st string, check 2nd */
              }  /* st_strcmp() */
              
              /*--------------------------------------------------------------------*/
              
              size_t st_strhash (const void *s, int type)
              {                               /* --- string hash function */
              register const char *p = (const char*)s; /* to traverse the key */
              register size_t     h  = (size_t)type;   /* hash value */
              /* Java: */
              /* while (*p) h = h *31 +(size_t)(unsigned char)*p++; */
              /* djb2: */
              /* h += 5381; */
              /* while (*p) h = h *33 +(size_t)(unsigned char)*p++; */
              /* sdbm: */
              /* while (*p) h = h *65599 +(size_t)(unsigned char)*p++; */
              /* own designs: */
              /* while (*p) h = h *61 +(size_t)(unsigned char)*p++; */
              while (*p) h = h *251 +(size_t)(unsigned char)*p++;
              /* while (*p) h = h *16777619 +(size_t)(unsigned char)*p++; */
              return h;                     /* compute and return hash value */
              }  /* st_strhash() */
              
              /*----------------------------------------------------------------------
              Name/Key Functions (integer)
              ----------------------------------------------------------------------*/
              
              int st_intcmp (const void *a, const void *b, void *data)
              {                               /* --- integer comparison function */
              return (*(const int*)a == *(const int*)b) ? 0 : -1;
              }  /* st_intcmp() */
              
              /*--------------------------------------------------------------------*/
              
              size_t st_inthash (const void *k, int type)
              {                               /* --- integer hash function */
              return (size_t)(*(const int*)k ^ type);
              }  /* st_inthash() */
              
              /*----------------------------------------------------------------------
              Name/Key Functions (long integer)
              ----------------------------------------------------------------------*/
              
              int st_lngcmp (const void *a, const void *b, void *data)
              {                               /* --- long comparison function */
              return (*(const long*)a == *(const long*)b) ? 0 : -1;
              }  /* st_lngcmp() */
              
              /*--------------------------------------------------------------------*/
              
              size_t st_lnghash (const void *k, int type)
              {                               /* --- long integer hash function */
              return (size_t)(*(const long*)k ^ (long)type);
              }  /* st_lnghash() */
              
              /*----------------------------------------------------------------------
              Name/Key Functions (size_t)
              ----------------------------------------------------------------------*/
              
              int st_sizcmp (const void *a, const void *b, void *data)
              {                               /* --- size_t comparison function */
              return (*(const size_t*)a == *(const size_t*)b) ? 0 : -1;
              }  /* st_sizcmp() */
              
              /*--------------------------------------------------------------------*/
              
              size_t st_sizhash (const void *k, int type)
              {                               /* --- long integer hash function */
              return (size_t)(*(const size_t*)k ^ (size_t)type);
              }  /* st_sizhash() */
              
              /*----------------------------------------------------------------------
              Name/Key Functions (ptrdiff_t)
              ----------------------------------------------------------------------*/
              
              int st_difcmp (const void *a, const void *b, void *data)
              {                               /* --- size_t comparison function */
              return (*(const ptrdiff_t*)a == *(const ptrdiff_t*)b) ? 0 : -1;
              }  /* st_difcmp() */
              
              /*--------------------------------------------------------------------*/
              
              size_t st_difhash (const void *k, int type)
              {                               /* --- long integer hash function */
              return (size_t)(*(const ptrdiff_t*)k ^ (ptrdiff_t)type);
              }  /* st_difhash() */
              
              /*----------------------------------------------------------------------
              Name/Key Functions (pointer/object)
              ----------------------------------------------------------------------*/
              
              int st_ptrcmp (const void *a, const void *b, void *data)
              {                               /* --- pointer comparison function */
              return (a == b) ? 0 : -1;     /* return whether pointers are equal */
              }  /* st_ptrcmp() */
              
              /*--------------------------------------------------------------------*/
              
              size_t st_ptrhash (const void *p, int type)
              {                               /* --- pointer hash function */
              return (size_t)*(const void**)p ^ (size_t)type;
              }  /* st_ptrhash() */
              
              /*----------------------------------------------------------------------
              Auxiliary Functions
              ----------------------------------------------------------------------*/
              
              static void delsym (SYMTAB *tab)
              {                               /* --- delete all symbols */
              size_t i;                     /* loop variable */
              STE    *e, *t;                /* to traverse the symbol list */
              
              assert(tab);                  /* check the function argument */
              for (i = 0; i < tab->size; i++) {
                e = tab->bins[i];           /* traverse the bin array */
              tab->bins[i] = NULL;        /* clear the current bin */
              while (e) {                 /* traverse the bin list */
              t = e; e = e->succ;       /* note the symbol and get next */
              if (tab->delfn) tab->delfn(t+1);
              free(t);                  /* if a deletion function is given, */
              }                           /* call it and then deallocate */
              }                             /* the symbol table element */
              }  /* delsym() */
              
              /*--------------------------------------------------------------------*/
              
              static STE* sort1 (STE *list)
              {                               /* --- sort a transaction list */
              STE *a, *b = list;            /* to traverse the lists */
              STE **e;                      /* end of the output list */
              
              for (a = list->succ; a; ) {   /* traverse the list to sort */
              a = a->succ;                /* two steps on a, one step on list */
              if (a) { a = a->succ; list = list->succ; }
              }                             /* (split list into two halves) */
              a = list->succ;               /* get the second list and */
              list->succ = NULL;            /* terminate the first list */
              if (a->succ) a = sort1(a);     /* and sort them recursively */
              if (b->succ) b = sort1(b);     /* if longer than one element */
              for (e = &list; 1; ) {        /* transaction list merge loop */
              if (a->level < b->level) {  /* copy element from first  list */
              *e = a; e = &a->succ; if (!(a = *e)) break; }
              else {                      /* copy element from second list */
              *e = b; e = &b->succ; if (!(b = *e)) break; }
              }                             /* (sort by visibility level) */
              *e = (a) ? a : b;             /* append the non-empty list */
              return list;                  /* return the sorted list */
              }  /* sort1() */
              
              /*--------------------------------------------------------------------*/
              
              static void rehash (SYMTAB *tab)
              {                               /* --- reorganize a hash table */
              size_t i, k, size;            /* loop variables, new bin array size */
              STE    **p, *e, *t;           /* to traverse symbol table elements */
              
              assert(tab);                  /* check the function argument */
              size = (tab->size << 1) +1;   /* calculate new bin array size */
              if ((size > tab->max)         /* if new size exceeds maximum */
              && ((size = tab->max) <= tab->size))
                return;                     /* set the maximal size */
              p = (STE**)calloc(size, sizeof(STE*));
              if (!p) return;               /* get an enlarged bin array */
              for (i = 0; i < tab->size; i++) {
                for (e = tab->bins[i]; e; ) {
                  t = e; e = e->succ;       /* traverse the old hash bins */
              k = tab->hashfn(t->key, t->type) % size;
              t->succ = p[k]; p[k] = t; /* compute the hash bin index */
                }                           /* and insert the symbol at */
              }                             /* the head of the bin list */
              free(tab->bins);              /* delete  the old bin array */
              tab->bins = p;                /* and set the new bin array */
              tab->size = size;             /* as well as its size */
              if (tab->level <= 0) return;  /* check for visibility levels */
              for (i = 0; i < size; i++)    /* traverse the new hash bins */
              if (p[i] && p[i]->succ)     /* sort all bin lists according */
              p[i] = sort1(p[i]);        /* to the visibility level */
              }  /* rehash() */
              
              /*----------------------------------------------------------------------
              Symbol Table Functions
              ----------------------------------------------------------------------*/
              
              SYMTAB* st_create (size_t init, size_t max, HASHFN hashfn,
                                 CMPFN cmpfn, void *data, OBJFN delfn)
              {                               /* --- create a symbol table */
              SYMTAB *tab;                  /* created symbol table */
              
              if (init <= 0) init = DFLT_INIT;  /* check and adapt the initial */
              if (max  <= 0) max  = DFLT_MAX;   /* and maximal bin array size */
              tab = (SYMTAB*)malloc(sizeof(SYMTAB));
              if (!tab) return NULL;        /* allocate symbol table body */
              tab->bins = (STE**)calloc(init, sizeof(STE*));
              if (!tab->bins) { free(tab); return NULL; }
              tab->level  = tab->cnt = 0;   /* allocate the hash bin array */
              tab->size   = init;           /* and initialize fields */
              tab->max    = max;            /* of symbol table body */
              tab->hashfn = (hashfn) ? hashfn : st_strhash;
              tab->cmpfn  = (cmpfn)  ? cmpfn  : st_strcmp;
              tab->data   = data;
              tab->delfn  = delfn;
              tab->idsize = (size_t)-1;
              tab->ids    = NULL;
              return tab;                   /* return created symbol table */
              }  /* st_create() */
              
              /*--------------------------------------------------------------------*/
              
              void st_delete (SYMTAB *tab)
              {                               /* --- delete a symbol table */
              assert(tab && tab->bins);     /* check argument */
              delsym(tab);                  /* delete all symbols, */
              free(tab->bins);              /* the hash bin array, */
              if (tab->ids) free(tab->ids); /* the identifier array, */
              free(tab);                    /* and the symbol table body */
              }  /* st_delete() */
              
              /*--------------------------------------------------------------------*/
              
              void* st_insert (SYMTAB *tab, const void *key, int type,
                               size_t keysize, size_t datasize)
              {                               /* --- insert a symbol (name/key) */
              size_t h;                     /* hash value */
              size_t i;                     /* index of hash bin, buffer */
              STE    *e, *n;                /* to traverse a bin list */
              
              assert(tab && key             /* check the function arguments */
              &&    ((datasize >= sizeof(int)) || (tab->idsize == (size_t)-1)));
              if ((tab->cnt  > tab->size)   /* if the bins are rather full and */
              &&  (tab->size < tab->max))   /* table does not have maximal size, */
              rehash(tab);                /* reorganize the hash table */
              h = tab->hashfn(key, type);   /* compute the hash value and */
              i = h % tab->size;            /* the index of the hash bin */
              for (e = tab->bins[i]; e; e = e->succ)
                if ((type == e->type) && (tab->cmpfn(key, e->key, tab->data) == 0))
                  break;                    /* check whether symbol exists */
              if (e && (e->level == tab->level))
                return EXISTS;              /* if symbol found on current level */
              
              /* if key/identifier map management */
              if (tab->cnt >= tab->idsize){ /* if the identifier array is full */
              IDENT  **p;                 /* (new) identifier array */
              size_t s = tab->idsize;     /* and its size */
              s += (s > BLKSIZE) ? s >> 1 : BLKSIZE;
              p  = (IDENT**)realloc(tab->ids, s *sizeof(IDENT*));
              if (!p) return NULL;        /* resize the identifier array and */
              tab->ids = p; tab->idsize = s;   /* set new array and its size */
              }                             /* (no resizing for symbol tables */
              /* since then tab->idsize = MAX_INT) */
              datasize = ((datasize +ALIGN-1) /ALIGN) *ALIGN;
              n = (STE*)malloc(sizeof(STE) +datasize +keysize);
              if (!n) return NULL;          /* allocate memory for new symbol */
              memcpy(n->key = (char*)(n+1) +datasize, key, keysize);
              n->type  = type;              /* note the symbol name/key, type, */
              n->level = tab->level;        /* and the current visibility level */
              n->succ  = tab->bins[i];      /* insert new symbol at the head */
              tab->bins[i] = n++;           /* of the hash bin list */
              /* if key/identifier maps are */
              if (tab->ids) {               /* supported and this is such a map */
              tab->ids[tab->cnt] = (IDENT*)n;
                *(IDENT*)n = (IDENT)tab->cnt;  /* store the new symbol */
              }                             /* in the identifier array */
              /* and set the symbol identifier */
              tab->cnt++;                   /* increment the symbol counter */
              return n;                     /* return pointer to data field */
              }  /* st_insert() */
              
              /*--------------------------------------------------------------------*/
              
              int st_remove (SYMTAB *tab, const void *key, int type)
              {                               /* --- remove a symbol/all symbols */
              size_t i;                     /* index of hash bin */
              STE    **p, *e;               /* to traverse a hash bin list */
              
              assert(tab);                  /* check the function arguments */
              if (!key) {                   /* if no symbol name/key given */
              delsym(tab);                /* delete all symbols */
              tab->cnt = tab->level = 0;  /* reset visibility level */
              return 0;                   /* and symbol counter */
              }                             /* and return 'ok' */
              i = tab->hashfn(key, type) % tab->size;
              p = tab->bins +i;             /* compute index of hash bin */
              while (*p) {                  /* and traverse the bin list */
              if (((*p)->type == type)    /* if the symbol was found */
              &&  (tab->cmpfn(key, (*p)->key, tab->data) == 0))
                break;                    /* abort the search loop */
              p = &(*p)->succ;            /* otherwise get the successor */
              }                             /* in the hash bin */
              e = *p;                       /* if the symbol does not exist, */
              if (!e) return -1;            /* abort the function with failure */
              *p = e->succ;                 /* remove symbol from hash bin */
              if (tab->delfn) tab->delfn(e+1);      /* delete user data */
              free(e);                      /* and symbol table element */
              tab->cnt--;                   /* decrement symbol counter */
              return 0;                     /* return 'ok' */
              }  /* st_remove() */
              
              /*--------------------------------------------------------------------*/
              
              void* st_lookup (SYMTAB *tab, const void *key, int type)
              {                               /* --- look up a symbol */
              size_t i;                     /* index of hash bin */
              STE    *e;                    /* to traverse a hash bin list */
              
              assert(tab && key);           /* check the function arguments */
              i = tab->hashfn(key, type) % tab->size;
              e = tab->bins[i];             /* compute index of hash bin */
              while (e) {                   /* and traverse the bin list */
              if ((e->type == type) && (tab->cmpfn(key, e->key, tab->data) == 0))
                return e +1;              /* if symbol found, return its data */
              e = e->succ;                /* otherwise get the successor */
              }                             /* in the hash bin */
              return NULL;                  /* return 'not found' */
              }  /* st_lookup() */
              
              /*----------------------------------------------------------------------
              Name/Identifier Map Functions
              ----------------------------------------------------------------------*/
              
              IDMAP* idm_create (size_t init, size_t max, HASHFN hashfn,
                                 CMPFN cmpfn, void *data, OBJFN delfn)
              {                               /* --- create a name/identifier map */
              IDMAP *idm;                   /* created name/identifier map */
              
              idm = st_create(init, max, hashfn, cmpfn, data, delfn);
              if (!idm) return NULL;        /* create a name/identifier map */
              idm->idsize = 0;              /* and clear the id. array size */
              return idm;                   /* return created name/id map */
              }  /* idm_create() */
              
              /*--------------------------------------------------------------------*/
              
              IDENT idm_getid (IDMAP *idm, const void *name)
              {                               /* --- get an item identifier */
              STE *p = (STE*)idm_bykey(idm, name);
                return (p) ? *(IDENT*)p : -1; /* look up the given name and */
              }  /* idm_getid() */            /* return its identifier or -1 */
              
              /*--------------------------------------------------------------------*/
              
              void idm_sort (IDMAP *idm, CMPFN cmpfn, void *data, IDENT *map, int dir)
              {                               /* --- sort name/identifier map */
              IDENT i;                      /* loop variable */
              IDENT **p;                    /* to traverse the value array */
              
              assert(idm && cmpfn);         /* check the function arguments */
              ptr_qsort(idm->ids, idm->cnt, +1, cmpfn, data);
              if (!map) {                   /* if no conversion map is requested */
              for (p = idm->ids +(i = (IDENT)idm->cnt); i > 0; )
                **--p = --i; }            /* just set new identifiers */
              else {                        /* if a conversion map is requested, */
              p = idm->ids +(i = (IDENT)idm->cnt);/* traverse the sorted array */
              if (dir < 0)                /* if backward map (i.e. new -> old) */
              while (i > 0) { map[--i] = **--p; **p = i; }
              else                        /* if forward  map (i.e. old -> new) */
              while (i > 0) { map[**--p] = --i; **p = i; }
              }                             /* (build conversion map) */
              }  /* idm_sort() */
              
              /*--------------------------------------------------------------------*/
              
              void idm_trunc (IDMAP *idm, size_t n)
              {                               /* --- truncate name/identifier map */
              IDENT *id;                    /* to access the identifiers */
              
              while (idm->cnt > n) {        /* while to remove mappings */
              id = idm->ids[idm->cnt-1];  /* get the identifier object */
              st_remove(idm, st_key(id), 0);
              }                             /* remove the symbol table element */
              }  /* idm_trunc() */
              
              /*----------------------------------------------------------------------
              Preprocessor Definitions
              ----------------------------------------------------------------------*/
#define ITEM        IDENT       /* item identifier type */
#define TID         int         /* transaction identifier type */
#define SUPP        TID         /* support type */
              
              
#define UITEM       unsigned int
#define ITEM_MIN    INT_MIN     /* minimum item identifier */
#define ITEM_MAX    INT_MAX     /* maximum item identifier */
#define ITEM_FMT    "d"         /* printf format code for int */
#define ia_qsort    int_qsort
#define ia_heapsort int_heapsort
#define ia_reverse  int_reverse
#define ia_unique   (ITEM)int_unique
#define ia_bsearch  (ITEM)int_bsearch
#define ia_bisect   (ITEM)int_bisect
              
#define TID_MAX     INT_MAX     /* maximum transaction identifier */
#define TID_FMT     "d"         /* printf format code for int */
              
              
#define SUPP_MIN    INT_MIN     /* minimum support value */
#define SUPP_MAX    INT_MAX     /* maximum support value */
#define SUPP_EPS    1           /* minimum support step */
#define SUPP_FMT    "d"         /* printf format code for int */
#define ceilsupp(s)     ceil(s)
#define floorsupp(s)    floor(s)
              
#define APP_NONE    0x00        /* item should be ignored */
#define APP_BODY    0x01        /* item may appear in rule body */
#define APP_HEAD    0x02        /* item may appear in rule head */
#define APP_BOTH    (APP_HEAD|APP_BODY)  /* item may apper in both */
              
              /* --- item base/transaction bag modes --- */
#define IB_WEIGHTS  0x20        /* items have t.a.-specific weights */
#define IB_OBJNAMES 0x40        /* item names are arbitrary objects */
              
              /* --- transaction sentinel --- */
#define TA_END      ITEM_MIN    /* sentinel for item instance arrays */
              
              /* --- transaction modes --- */
#define TA_PACKED   0x1f        /* transactions have been packed */
#define TA_EQPACK   0x20        /* treat packed items all the same */
#define TA_HEAP     0x40        /* prefer heap sort to quicksort */
              
              /* --- transaction read/write modes --- */
#define TA_WEIGHT   0x01        /* integer weight in last field */
#define TA_DUPLICS  0x02        /* allow duplicates of items */
#define TA_DUPERR   0x04        /* consider duplicates as errors */
#define TA_TERM     0x10        /* terminate all trans. with item 0 */
#define TA_WGTSEP   TRD_OTHER   /* item weight separator */
#define TA_PAREN    0x02        /* print parentheses around weight */
              
              /* --- idempotent weight modes --- */
#define TA_NOGAPS   0x40        /* do not allow gaps in matching */
#define TA_ALLOCC   0x80        /* consider all occurrences */
              
              /* --- error codes --- */
#define E_NONE         0        /* no error */
#define E_NOMEM      (-1)       /* not enough memory */
#define E_FOPEN      (-2)       /* cannot open file */
#define E_FREAD      (-3)       /* read error on file */
#define E_FWRITE     (-4)       /* write error on file */
              
#define E_NOITEMS   (-15)       /* no frequent items found */
#define E_ITEMEXP   (-16)       /* item expected */
#define E_ITEMWGT   (-17)       /* invalid item weight */
#define E_DUPITEM   (-18)       /* duplicate item */
#define E_INVITEM   (-19)       /* invalid item (not integer) */
#define E_WGTEXP    (-20)       /* transaction weight expected */
#define E_TAWGT     (-21)       /* invalid transaction weight */
#define E_FLDCNT    (-22)       /* too many fields */
#define E_APPEXP    (-23)       /* appearance indicator expected */
#define E_UNKAPP    (-24)       /* unknown appearance indicator */
#define E_PENEXP    (-25)       /* insertion penalty expected */
#define E_PENALTY   (-26)       /* invalid insertion penalty */
              
              /* --- special macros --- */
#define ispacked(i) (((i) ^ TA_END) > 0)
              
              /*----------------------------------------------------------------------
              Type Definitions
----------------------------------------------------------------------*/
              typedef struct {                /* --- item data --- */
              ITEM     id;                  /* item identifier */
              int      app;                 /* appearance indicator */
              double   pen;                 /* insertion penalty */
              SUPP     frq;                 /* standard frequency (trans. weight) */
              SUPP     xfq;                 /* extended frequency (trans. sizes) */
              TID      idx;                 /* index of counted last transaction */
              } ITEMDATA;                     /* (item data) */
              
              typedef struct {                /* --- item base --- */
              IDMAP    *idmap;              /* key/name to identifier map */
              SUPP     wgt;                 /* total weight of transactions */
              SUPP     max;                 /* maximum support of an item */
              int      mode;                /* mode (IB_WEIGHTS, IB_OBJNAMES) */
              int      app;                 /* default appearance indicator */
              double   pen;                 /* default insertion penalty */
              TID      idx;                 /* index of current transaction */
              ITEM     size;                /* size of the transaction buffer */
              void     *tract;              /* buffer for a transaction */
              int      err;                 /* error code (file reading) */
              void     *trd;                /* placeholder (for fixed size) */
              } ITEMBASE;                     /* (item base) */
              
              typedef struct {                /* --- transaction --- */
              SUPP     wgt;                 /* weight (number of occurrences) */
              ITEM     size;                /* size   (number of items) */
              TID      mark;                /* mark   (e.g. bit coded items) */
              ITEM     items[1];            /* items in the transaction */
              } TRACT;                        /* (transaction) */
              
              typedef struct {                /* --- weighted item instance --- */
              ITEM     item;                /* item identifier */
              float    wgt;                 /* item weight (transaction-specific) */
              } WITEM;                        /* (weighted item instance) */
              
              typedef struct {                /* --- trans. with weighted items --- */
              SUPP     wgt;                 /* weight (number of occurrences) */
              ITEM     size;                /* size   (number of items) */
              int      mark;                /* mark   (e.g. bit coded items) */
              WITEM    items[1];            /* items in the transaction */
              } WTRACT;                       /* (transaction with weighted items) */
              
              typedef struct {                /* --- transaction bag/multiset --- */
              ITEMBASE *base;               /* underlying item base */
              int      mode;                /* mode (IB_OBJNAMES, IB_WEIGHT) */
              ITEM     max;                 /* number of items in largest trans. */
              SUPP     wgt;                 /* total weight of transactions */
              size_t   extent;              /* total number of item instances */
              TID      size;                /* size of the transaction array */
              TID      cnt;                 /* number of transactions */
              void     **tracts;            /* array  of transactions */
              TID      *icnts;              /* number of transactions per item */
              SUPP     *ifrqs;              /* frequency of the items (weight) */
              void     *buf;                /* buffer for surrogate generation */
              } TABAG;                        /* (transaction bag/multiset) */
              typedef struct {                /* --- transaction tree node --- */
              SUPP     wgt;                 /* weight (number of transactions) */
              ITEM     max;                 /* number of items in largest trans. */
              ITEM     size;                /* node size (number of children) */
              ITEM     items[1];            /* next items in rep. transactions */
              } TANODE;                       /* (transaction tree node) */
              
              typedef struct {                /* --- transaction tree --- */
              TABAG    *bag;                /* underlying transaction bag */
              TANODE   *root;               /* root of the transaction tree */
              TANODE   empty;               /* empty transaction node */
              } TATREE;                       /* (transaction tree) */
              
              typedef TABAG* TBGSURRFN (TABAG *src, RNG *rng, TABAG *dst);
              
              /*----------------------------------------------------------------------
              Item Base Functions
              ----------------------------------------------------------------------*/
              ITEMBASE*    ib_create   (int mode, ITEM size, ...);
              void         ib_delete   (ITEMBASE *base);
              
              int          ib_mode     (ITEMBASE *base);
              ITEM         ib_cnt      (ITEMBASE *base);
              ITEM         ib_add      (ITEMBASE *base, const void *name);
              ITEM         ib_item     (ITEMBASE *base, const void *name);
              const char*  ib_name     (ITEMBASE *base, ITEM item);
              const void*  ib_key      (ITEMBASE *base, ITEM item);
              const void*  ib_obj      (ITEMBASE *base, ITEM item);
              const char*  ib_xname    (ITEMBASE *base, ITEM item);
              void         ib_clear    (ITEMBASE *base);
              ITEM         ib_add2ta   (ITEMBASE *base, const void *name);
              void         ib_finta    (ITEMBASE *base, SUPP wgt);
              
              SUPP         ib_getwgt   (ITEMBASE *base);
              SUPP         ib_setwgt   (ITEMBASE *base, SUPP wgt);
              SUPP         ib_incwgt   (ITEMBASE *base, SUPP wgt);
              
              int          ib_getapp   (ITEMBASE *base, ITEM item);
              SUPP         ib_getfrq   (ITEMBASE *base, ITEM item);
              SUPP         ib_setfrq   (ITEMBASE *base, ITEM item, SUPP frq);
              SUPP         ib_incfrq   (ITEMBASE *base, ITEM item, SUPP frq);
              SUPP         ib_getxfq   (ITEMBASE *base, ITEM item);
              SUPP         ib_setxfq   (ITEMBASE *base, ITEM item, SUPP xfq);
              SUPP         ib_incxfq   (ITEMBASE *base, ITEM item, SUPP xfq);
              double       ib_getpen   (ITEMBASE *base, ITEM item);
              double       ib_setpen   (ITEMBASE *base, ITEM item, double pen);
              SUPP         ib_maxfrq   (ITEMBASE *base);
              
              ITEM         ib_recode   (ITEMBASE *base, SUPP min, SUPP max,
                                        ITEM cnt, int dir, ITEM *map);
              
              TRACT*       ib_tract    (ITEMBASE *base);
              WTRACT*      ib_wtract   (ITEMBASE *base);
              
              
              /*----------------------------------------------------------------------
              Transaction Functions
              ----------------------------------------------------------------------*/
              TRACT*       ta_create   (const ITEM *items, ITEM n, SUPP wgt);
              void         ta_delete   (TRACT *t);
              TRACT*       ta_clone    (const TRACT *t);
              TRACT*       ta_copy     (TRACT *dst, const TRACT *src);
              
              const ITEM*  ta_items    (const TRACT *t);
              ITEM         ta_size     (const TRACT *t);
              SUPP         ta_wgt      (const TRACT *t);
              int          ta_setmark  (TRACT *t, int mark);
              int          ta_getmark  (const TRACT *t);
              int          ta_bitmark  (TRACT *t);
              
              void         ta_sort     (TRACT *t, int dir);
              void         ta_reverse  (TRACT *t);
              ITEM         ta_unique   (TRACT *t);
              ITEM         ta_pack     (TRACT *t, int n);
              ITEM         ta_unpack   (TRACT *t, int dir);
              
              int          ta_equal    (const TRACT *t1, const TRACT *t2);
              int          ta_cmp      (const void *p1,
                                        const void *p2, void *data);
              int          ta_cmpep    (const void *p1,
                                        const void *p2, void *data);
              int          ta_cmpoff   (const void *p1,
                                        const void *p2, void *data);
              int          ta_cmplim   (const void *p1,
                                        const void *p2, void *data);
              int          ta_cmpsfx   (const void *p1,
                                        const void *p2, void *data);
              int          ta_cmpx     (const TRACT *t,
                                        const ITEM *items, ITEM n);
              int          ta_cmpsz    (const void *p1,
                                        const void *p2, void *data);
              ITEM         ta_subset   (const TRACT *t1,
                                        const TRACT *t2, ITEM off);
              ITEM         ta_subwog   (const TRACT *t1,
                                        const TRACT *t2, ITEM off);
              
              
              /*----------------------------------------------------------------------
              Weighted Item Instance Functions
              ----------------------------------------------------------------------*/
              int          wi_cmp      (const WITEM *a, const WITEM *b);
              int          wi_cmpw     (const WITEM *a, const WITEM *b);
              void         wi_sort     (WITEM *wia, ITEM n, int dir);
              void         wi_reverse  (WITEM *wia, ITEM n);
              ITEM         wi_unique   (WITEM *wia, ITEM n);
              
              /*----------------------------------------------------------------------
              Extended Transaction Functions
              ----------------------------------------------------------------------*/
              void         wta_delete  (WTRACT *t);
              WTRACT*      wta_clone   (const WTRACT *t);
              WTRACT*      wta_copy    (WTRACT *dst, const WTRACT *src);
              const WITEM* wta_items   (const WTRACT *t);
              ITEM         wta_size    (const WTRACT *t);
              SUPP         wta_wgt     (const WTRACT *t);
              
              int          wta_cmp     (const void *p1,
                                        const void *p2, void *data);
              
              /*----------------------------------------------------------------------
              Transaction Bag/Multiset Functions
              ----------------------------------------------------------------------*/
              TABAG*       tbg_create  (ITEMBASE *base);
              void         tbg_delete  (TABAG *bag, int delib);
              ITEMBASE*    tbg_base    (TABAG *bag);
              TABAG*       tbg_clone   (TABAG *bag);
              TABAG*       tbg_copy    (TABAG *dst, TABAG *src);
              
              ITEM         tbg_itemcnt (const TABAG *bag);
              TID          tbg_cnt     (const TABAG *bag);
              SUPP         tbg_wgt     (const TABAG *bag);
              ITEM         tbg_max     (const TABAG *bag);
              size_t       tbg_extent  (const TABAG *bag);
              const TID*   tbg_icnts   (TABAG *bag, int recnt);
              const SUPP*  tbg_ifrqs   (TABAG *bag, int recnt);
              
              int          tbg_add     (TABAG *bag,  TRACT *t);
              int          tbg_addw    (TABAG *bag, WTRACT *t);
              int          tbg_addib   (TABAG *bag);
              TRACT*       tbg_tract   (TABAG *bag, TID index);
              WTRACT*      tbg_wtract  (TABAG *bag, TID index);
              int          tbg_istab   (TABAG *bag);
              ITEM         tbg_recode  (TABAG *bag, SUPP min, SUPP max,
                                        ITEM cnt, int dir);
              void         tbg_filter  (TABAG *bag, ITEM min,
                                        const int *marks, double wgt);
              void         tbg_itsort  (TABAG *bag, int dir, int heap);
              void         tbg_sort    (TABAG *bag, int dir, int heap);
              TID          tbg_reduce  (TABAG *bag, int keep0);
              void         tbg_pack    (TABAG *bag, int n);
              void         tbg_unpack  (TABAG *bag, int dir);
              
              /*----------------------------------------------------------------------
              Transaction Node Functions
              ----------------------------------------------------------------------*/
              SUPP         tan_wgt     (const TANODE *node);
              ITEM         tan_max     (const TANODE *node);
              ITEM         tan_size    (const TANODE *node);
              ITEM*        tan_items   (TANODE *node);
              ITEM         tan_item    (const TANODE *node, ITEM index);
              TANODE*      tan_child   (const TANODE *node, ITEM index);
              /*----------------------------------------------------------------------
              Transaction Tree Functions
              ----------------------------------------------------------------------*/
              
              TANODE*      tat_root    (const TATREE *tree);
              
              
              /*----------------------------------------------------------------------
              Preprocessor Definitions
              ----------------------------------------------------------------------*/
#define ib_mode(s)        ((s)->mode)
#define ib_cnt(s)         ((ITEM)idm_cnt((s)->idmap))
#define ib_item(s,n)      idm_getid((s)->idmap, n)
#define ib_name(s,i)      idm_name(idm_byid((s)->idmap, i))
#define ib_key(s,i)       idm_key (idm_byid((s)->idmap, i))
#define ib_obj(s,i)       (*(void**)idm_key(idm_byid((s)->idmap, i)))
              
#define ib_getwgt(s)      ((s)->wgt)
#define ib_setwgt(s,n)    ((s)->wgt  = (n))
#define ib_incwgt(s,n)    ((s)->wgt += (n))
              
#define ib_itemdata(s,i)  ((ITEMDATA*)idm_byid((s)->idmap, i))
#define ib_getfrq(s,i)    (ib_itemdata(s,i)->frq)
#define ib_setfrq(s,i,n)  (ib_itemdata(s,i)->frq  = (n))
#define ib_incfrq(s,i,n)  (ib_itemdata(s,i)->frq += (n))
#define ib_getxfq(s,i)    (ib_itemdata(s,i)->xfq)
#define ib_setxfq(s,i,n)  (ib_itemdata(s,i)->xfq  = (n))
#define ib_incxfq(s,i,n)  (ib_itemdata(s,i)->xfq += (n))
#define ib_getpen(s,i)    (ib_itemdata(s,i)->pen)
#define ib_setpen(s,i,p)  (ib_itemdata(s,i)->pen  = (p))
#define ib_maxfrq(s)      ((s)->max)
              
#define ib_tract(s)       ((TRACT*) (s)->tract)
#define ib_wtract(s)      ((WTRACT*)(s)->tract)
              
              /*--------------------------------------------------------------------*/
#define ta_delete(t)      free(t)
#define ta_items(t)       ((const ITEM*)(t)->items)
#define ta_size(t)        ((t)->size)
#define ta_wgt(t)         ((t)->wgt)
#define ta_setmark(t,m)   ((t)->mark = (m))
#define ta_getmark(t)     ((t)->mark)
              
              /*--------------------------------------------------------------------*/
#define wta_delete(t)     free(t)
#define wta_items(t)      ((t)->items)
#define wta_size(t)       ((t)->size)
#define wta_wgt(t)        ((t)->wgt)
              
              /*--------------------------------------------------------------------*/
#define tbg_base(b)       ((b)->base)
              
#define tbg_itemcnt(b)    ib_cnt((b)->base)
#define tbg_cnt(b)        ((b)->cnt)
#define tbg_wgt(b)        ((b)->wgt)
#define tbg_max(b)        ((b)->max)
#define tbg_extent(b)     ((b)->extent)
              
#define tbg_tract(b,i)    ((TRACT*) (b)->tracts[i])
#define tbg_wtract(b,i)   ((WTRACT*)(b)->tracts[i])
              
              /*--------------------------------------------------------------------*/
              
#define tan_wgt(n)        ((n)->wgt)
#define tan_max(n)        ((n)->max)
#define tan_size(n)       ((n)->size)
#define tan_item(n,i)     ((n)->items[i])
#define tan_items(n)      ((n)->items)
              
#define tat_root(t)       ((t)->root)
              
#define PAD(p)      0           /* no padding is needed */
#define ALIGN(p)
              
              /*----------------------------------------------------------------------
              Preprocessor Definitions
----------------------------------------------------------------------*/
#define E_STDIN      (-5)       /* double assignment of stdin */
#define E_OPTION     (-6)       /* unknown option */
#define E_OPTARG     (-7)       /* missing option argument */
#define E_ARGCNT     (-8)       /* too few/many arguments */
#define E_ITEMCNT    (-9)       /* invalid number of items */
              /* error codes -15 to -25 defined in tract.h */
              
#define BLKSIZE      1024       /* block size for enlarging arrays */
#define TH_INSERT       8       /* threshold for insertion sort */
#define TS_PRIMES    (sizeof(primes)/sizeof(*primes))
              
#define MSG         fprintf     /* print messages */
#define CLOCK(t)    ((t) = clock())
              
#define SEC_SINCE(t)  ((double)(clock()-(t)) /(double)CLOCKS_PER_SEC)
              
#define CCHAR const char        /* abbreviation */
              
              /*----------------------------------------------------------------------
              Type Definitions
----------------------------------------------------------------------*/
              typedef struct {                /* --- item frequency --- */
              ITEM item;                    /* item identifier */
              SUPP frq;                     /* number of occurrences */
              SUPP dif;                     /* difference to original */
              } ITEMFRQ;                      /* (item frequency) */
              
              typedef ITEM SUBFN  (const TRACT  *t1, const TRACT  *t2, ITEM off);
              typedef ITEM SUBWFN (const WTRACT *t1, const WTRACT *t2, ITEM off);
              
              /*----------------------------------------------------------------------
              Constants
              ----------------------------------------------------------------------*/
              static WITEM  WTA_END  = {-1,0};/* sentinel for weighted items */
              
              static int nocmp (const void *p1, const void *p2, void *data)
              {                               /* --- compare item identifiers */
              const ITEMDATA *a = (const ITEMDATA*)p1; /* type the pointers */
              const ITEMDATA *b = (const ITEMDATA*)p2;
              
              if (a->app == APP_NONE) return (b->app == APP_NONE) ? 0 : 1;
              if (b->app == APP_NONE) return -1;
              if (a->id  >  b->id)    return +1;
              if (a->id  <  b->id)    return -1;
              return 0;                     /* return sign of identifier diff. */
              }  /* nocmp() */
              
              /*--------------------------------------------------------------------*/
              
              static int asccmp (const void *p1, const void *p2, void *data)
              {                               /* --- compare item frequencies */
              const ITEMDATA *a = (const ITEMDATA*)p1; /* type the pointers */
              const ITEMDATA *b = (const ITEMDATA*)p2;
              
              if (a->app == APP_NONE) return (b->app == APP_NONE) ? 0 : 1;
              if (b->app == APP_NONE) return -1;
              if (a->frq >  b->frq)   return +1;
              if (a->frq <  b->frq)   return -1;
              return 0;                     /* return sign of frequency diff. */
              }  /* asccmp() */
              
              /*--------------------------------------------------------------------*/
              
              static int descmp (const void *p1, const void *p2, void *data)
              {                               /* --- compare item frequencies */
              const ITEMDATA *a = (const ITEMDATA*)p1; /* type the pointers */
              const ITEMDATA *b = (const ITEMDATA*)p2;
              
              if (a->app == APP_NONE) return (b->app == APP_NONE) ? 0 : 1;
              if (b->app == APP_NONE) return -1;
              if (a->frq <  b->frq)   return +1;
              if (a->frq >  b->frq)   return -1;
              return 0;                     /* return sign of frequency diff. */
              }  /* descmp() */
              
              /*--------------------------------------------------------------------*/
              
              static int asccmpx (const void *p1, const void *p2, void *data)
              {                               /* --- compare item frequencies */
              const ITEMDATA *a = (const ITEMDATA*)p1; /* type the pointers */
              const ITEMDATA *b = (const ITEMDATA*)p2;
              
              if (a->app == APP_NONE) return (b->app == APP_NONE) ? 0 : 1;
              if (b->app == APP_NONE) return -1;
              if (a->xfq >  b->xfq)   return +1;
              if (a->xfq <  b->xfq)   return -1;
              return 0;                     /* return sign of frequency diff. */
              }  /* asccmpx() */
              
              /*--------------------------------------------------------------------*/
              
              static int descmpx (const void *p1, const void *p2, void *data)
              {                               /* --- compare item frequencies */
              const ITEMDATA *a = (const ITEMDATA*)p1; /* type the pointers */
              const ITEMDATA *b = (const ITEMDATA*)p2;
              
              if (a->app == APP_NONE) return (b->app == APP_NONE) ? 0 : 1;
              if (b->app == APP_NONE) return -1;
              if (a->xfq <  b->xfq)   return +1;
              if (a->xfq >  b->xfq)   return -1;
              return 0;                     /* return sign of frequency diff. */
              }  /* descmpx() */
              
              /*----------------------------------------------------------------------
              Item Base Functions
              ----------------------------------------------------------------------*/
              
              ITEMBASE* ib_create (int mode, ITEM size, ...)
              {                               /* --- create an item base */
              ITEMBASE *base;               /* created item base */
              TRACT    *t;                  /* a transaction */
              WTRACT   *x;                  /* a transaction with weighted items */
              va_list  args;                /* list of variable arguments */
              HASHFN   *hashfn;             /* hash function for general objects */
              CMPFN    *cmpfn;              /* ditto, comparison function */
              void     *data;               /* data for comparison function */
              OBJFN    *delfn;              /* ditto, deletion function */
              
              if (size <= 0) size = BLKSIZE;/* check and adapt number of items */
              base = (ITEMBASE*)malloc(sizeof(ITEMBASE));
              if (!base) return NULL;       /* create item base and components */
              if (mode & IB_OBJNAMES) {     /* if general objects as names, */
              va_start(args, size);       /* get the necessary arguments */
              hashfn = va_arg(args, HASHFN*);
              cmpfn  = va_arg(args, CMPFN*);
              data   = va_arg(args, void*);
              delfn  = va_arg(args, OBJFN*);
              va_end(args);               /* create an item identifier map */
              base->idmap = idm_create(8191, 0, hashfn, cmpfn, data, delfn); }
              else                          /* if item names are strings */
              base->idmap = idm_create(8191, 0, ST_STRFN, (OBJFN*)0);
              if (!base->idmap) { free(base); return NULL; }
              base->mode = mode;            /* initialize the fields */
              base->wgt  = base->max = 0;   /* there are no transactions yet */
              base->app  = APP_BOTH;        /* default: appearance in body & head */
              base->pen  = 0.0;             /* default: no item insertion allowed */
              base->idx  = 1;               /* index of current transaction */
              base->size = size;            /* size of transaction buffer */
              if (mode & IB_WEIGHTS) {      /* if items carry weights */
              base->tract = x = (WTRACT*)malloc(                 sizeof(WTRACT)
                                                                   +(size_t)(size+1) *sizeof(WITEM));
                if (x) { x->wgt = 0; x->size = 0; x->mark = 0;
                x->items[size+1] = x->items[0] = WTA_END; } }
              else {                        /* if items do not carry weights */
              base->tract = t = (TRACT*) malloc(                 sizeof(TRACT)
                                                                   +(size_t)(size+1) *sizeof(ITEM));
                if (t) { t->wgt = 0; t->size = 0; t->mark = 0;
                t->items[size+1] = t->items[0] = TA_END; }
              }                             /* clear the transaction buffer */
              if (!base->tract) { ib_delete(base); return NULL; }
              return base;                  /* return the created item set */
              }  /* ib_create() */
              
              /*--------------------------------------------------------------------*/
              
              void ib_delete (ITEMBASE *base)
              {                               /* --- delete an item set */
              assert(base);                 /* check the function argument */
              if (base->tract) free(base->tract);
              if (base->idmap) idm_delete(base->idmap);
              free(base);                   /* delete the components */
              }  /* ib_delete() */            /* and the item base body */
              
              /*--------------------------------------------------------------------*/
              
              ITEM ib_add (ITEMBASE *base, const void *name)
              {                               /* --- add an item to the set */
              size_t   size;                /* size of the item name */
              ITEMDATA *itd;                /* to access the item data */
              
              assert(base && name);         /* check the function arguments */
              size = (base->mode & IB_OBJNAMES) ? sizeof(const void*)
                : strlen((const char*)name)+1;
              itd = (ITEMDATA*)idm_add(base->idmap, name, size, sizeof(ITEMDATA));
              if (itd == NULL)   return -1; /* add the new item */
              if (itd == EXISTS) return -2; /* to the identifier map */
              itd->app = base->app;         /* init. the appearance indicator */
              itd->xfq = itd->frq = 0;      /* clear the support counters */
              itd->idx = 0;                 /* and the transaction index */
              itd->pen = base->pen;         /* init. the insertion penalty */
              return itd->id;               /* return the item identifier */
              }  /* ib_add() */
              
              /*--------------------------------------------------------------------*/
              
              const char* ib_xname (ITEMBASE *base, ITEM item)
              {                               /* --- get an item name */
              static char buf[32];          /* buffer for integer formatting */
              
              assert(base && (item >= 0));  /* check the function arguments */
              if (!(base->mode & IB_OBJNAMES))
                return ib_name(base, item); /* if possible, return name directly */
              snprintf(buf, sizeof(buf), "%p", ib_name(base, item));
              return buf;                   /* format the object pointer and */
              }  /* ib_xname() */             /* return the formatting buffer */
              
              /*--------------------------------------------------------------------*/
              
              void ib_clear (ITEMBASE *base)
              {                               /* --- clear buffered transaction */
              ((TRACT*)base->tract)->size = 0;
                base->idx += 1;               /* reset the number of items and */
              }  /* ib_clear() */             /* update the transaction index */
              
              /*--------------------------------------------------------------------*/
              
              ITEM ib_add2ta (ITEMBASE *base, const void *name)
              {                               /* --- add an item to transaction */
              size_t   size;                /* size of item name/buffer */
              ITEMDATA *itd;                /* to access the item data */
              TRACT    *t;                  /* to access the transaction */
              ITEM     n;                   /* size of transaction buffer */
              
              assert(base && name);         /* check the function arguments */
              itd = (ITEMDATA*)idm_bykey(base->idmap, name);
              if (!itd) {                   /* get the item by its key/name */
              size = (base->mode & IB_OBJNAMES) ? sizeof(const void*)
                : strlen((const char*)name)+1;
                itd = (ITEMDATA*)idm_add(base->idmap, name, size, sizeof(ITEMDATA));
                if (itd == NULL) return -1; /* add a new item to the base */
              itd->app = base->app;       /* init. the appearance indicator */
              itd->xfq = itd->frq = 0;    /* clear the support counters */
              itd->idx = 0;               /* and the transaction index */
              itd->pen = base->pen;       /* init. the insertion penalty */
              }
              t = (TRACT*)base->tract;      /* get the transaction buffer */
              if (itd->idx >= base->idx)    /* if the item is already contained, */
              return t->size;             /* simply abort the function */
              itd->idx = base->idx;         /* update the transaction index */
              n = base->size;               /* get the current buffer size */
              if (t->size >= n) {           /* if the transaction buffer is full */
              n += (n > BLKSIZE) ? (n >> 1) : BLKSIZE;
                t  = (TRACT*)realloc(t, sizeof(TRACT) +(size_t)n *sizeof(ITEM));
                if (!t) return -1;          /* enlarge the transaction buffer */
              t->items[base->size = n] = TA_END; base->tract = t;
              }                             /* set the new buffer and its size */
              t->items[t->size] = itd->id;  /* store the new item */
              return ++t->size;             /* return the new transaction size */
              }  /* ib_add2ta() */
              
              /*--------------------------------------------------------------------*/
              
              void ib_finta (ITEMBASE *base, SUPP wgt)
              {                               /* --- finalize transaction buffer */
              ITEM     i;                   /* loop variable */
              TRACT    *t;                  /* to access the transaction buffer */
              ITEMDATA *itd;                /* to access the item data */
              
              assert(base);                 /* check the function arguments */
              t = (TRACT*)base->tract;      /* get the transaction buffer and */
              t->items[t->size] = TA_END;   /* store a sentinel at the end */
              base->wgt += t->wgt = wgt;    /* sum the transaction weight and */
              wgt *= (SUPP)t->size;         /* compute extended frequency weight */
              for (i = 0; i < t->size; i++) {
                itd = (ITEMDATA*)idm_byid(base->idmap, t->items[i]);
                itd->xfq += wgt;            /* traverse the items and */
              itd->frq += t->wgt;         /* sum the transaction weights */
              if (itd->frq > base->max) base->max = itd->frq;
              }                             /* update maximum item support */
              }  /* ib_finta() */
              
              /*--------------------------------------------------------------------*/
              
              int ib_getapp (ITEMBASE *base, ITEM item)
              {                               /* --- get item appearance */
              assert(base);                 /* check the function arguments */
              if (item < 0) return base->app;
              return ((ITEMDATA*)idm_byid(base->idmap, item))->app;
              }  /* ib_getapp() */
              
              ITEM ib_recode (ITEMBASE *base, SUPP min, SUPP max,
                              ITEM cnt, int dir, ITEM *map)
              {                               /* --- recode items w.r.t. frequency */
              ITEM     k, n;                /* loop variables */
              ITEM     i;                   /* item buffer */
              ITEMDATA *itd;                /* to traverse the items */
              TRACT    *t;                  /* to access the standard transaction */
              ITEM     *s, *d;              /* to traverse the items */
              WTRACT   *x;                  /* to access the extended transaction */
              WITEM    *a, *b;              /* to traverse the items */
              CMPFN    *cmp;                /* comparison function */
              
              assert(base);                 /* check the function arguments */
              if (max < 0) max = SUPP_MAX;  /* adapt the maximum frequency */
              if (cnt < 0) cnt = ITEM_MAX;  /* adapt the maximum number of items */
              for (n = idm_cnt(base->idmap); --n >= 0; ) {
                itd = (ITEMDATA*)idm_byid(base->idmap, n);
                if ((itd->frq < min) || (itd->frq > max))
                  itd->app = APP_NONE;      /* set all items to 'ignore' */
              }                             /* that have an invalid frequency */
              if      (dir >  1) cmp = asccmpx;  /* get the appropriate */
              else if (dir >  0) cmp = asccmp;   /* comparison function */
              else if (dir >= 0) cmp = nocmp;    /* (ascending/descending) */
              else if (dir > -2) cmp = descmp;   /* and sort the items */
              else               cmp = descmpx;  /* w.r.t. their frequency */
              idm_sort(base->idmap, cmp, NULL, map, 1);
              for (k = n = idm_cnt(base->idmap); n > 0; n--)
                if (((ITEMDATA*)idm_byid(base->idmap, n-1))->app != APP_NONE)
                  break;                    /* find non-ignorable items */
              if (n > cnt) n = cnt;         /* limit the number of items */
              idm_trunc(base->idmap, (size_t)n); /* remove all items to ignore */
              if (!map) return n;           /* if no map is provided, abort */
              while (k > 0)                 /* mark all removed items */
              if (map[--k] >= n) map[k] = -1;
              if (base->mode & IB_WEIGHTS){ /* if the items carry weights */
              x = (WTRACT*)base->tract;   /* traverse the buffered transaction */
              for (a = b = x->items; a->item >= 0; a++) {
                i = map[a->item];         /* recode all items and remove */
              if (i >= 0) (b++)->item = i;      /* all items to ignore */
              }                           /* from the buffered transaction */
              x->size = (ITEM)(b -x->items); /* compute the new number of items */
              x->items[x->size] = WTA_END; } /* store sentinel after the items */
              else {                        /* if the items do not carry weights */
              t = (TRACT*)base->tract;    /* traverse the buffered transaction */
              for (s = d = t->items; *s > TA_END; s++) {
                i = map[*s];              /* recode all items and */
              if (i >= 0) *d++ = i;     /* remove all items to ignore */
              }                           /* from the buffered transaction */
              t->size = (ITEM)(d -t->items);
              t->items[t->size] = TA_END; /* compute the new number of items */
              }                             /* store a sentinel after the items */
              return n;                     /* return number of frequent items */
              }  /* ib_recode() */
              /*----------------------------------------------------------------------
              Transaction Functions
              ----------------------------------------------------------------------*/
              
              TRACT* ta_create (const ITEM *items, ITEM n, SUPP wgt)
              {                               /* --- create a transaction */
              TRACT *t;                     /* created transaction */
              
              assert(items || (n <= 0));    /* check the function arguments */
              t = (TRACT*)malloc(sizeof(TRACT) +(size_t)n *sizeof(ITEM));
              if (!t) return NULL;          /* allocate a new transaction */
              t->wgt  = wgt;                /* set weight, size and marker */
              t->size = n; t->mark = 0;     /* and copy the items */
              memcpy(t->items, items, (size_t)n *sizeof(ITEM));
              t->items[n] = TA_END;         /* store a sentinel after the items */
              return t;                     /* and return the created transaction */
              }  /* ta_create() */
              
              /*--------------------------------------------------------------------*/
              
              TRACT* ta_clone (const TRACT *t)
              { return ta_create(t->items, t->size, t->wgt); }
              
              /*--------------------------------------------------------------------*/
              
              TRACT* ta_copy (TRACT *dst, const TRACT *src)
              {                               /* --- copy a transaction */
              assert(dst                    /* check the function arguments */
              &&     src && (dst->size == src->size));
                dst->wgt = src->wgt;          /* copy weight and items */
              memcpy(dst->items, src->items, (size_t)(src->size+1) *sizeof(ITEM));
              return dst;                   /* return the destination */
              }  /* ta_copy() */
              
              /*--------------------------------------------------------------------*/
              
              int ta_bitmark (TRACT *t)
              {                               /* --- set mark to item bits */
              int  mark;                    /* bits representing items 0 to 31 */
              ITEM *s;                      /* to traverse the items */
              
              assert(t);                    /* check the function argument */
              for (mark = 0, s = t->items; *s > TA_END; s++) {
                if      (*s <  0) mark |= *s & ~TA_END;
                else if (*s < 32) mark |= 1 << *s;
              }                             /* set bits for items 0 to 31 */
              return t->mark = mark;        /* store and return the item bits */
              }  /* ta_bitmark() */
              
              /*--------------------------------------------------------------------*/
              
              void ta_sort (TRACT *t, int dir)
              {                               /* --- sort items in transaction */
              ITEM n;                       /* number of items */
              
              assert(t);                    /* check the function argument */
              if ((n = t->size) < 2) return;/* do not sort less than two items */
              while ((n > 0) && (t->items[n-1] <= TA_END))
                --n;                        /* skip additional end markers and */
              ia_qsort(t->items, (size_t)n, dir);  /* sort the remaining items */
              }  /* ta_sort() */
              
              /*--------------------------------------------------------------------*/
              
              void ta_reverse (TRACT *t)
              {                               /* --- reverse items in transaction */
              ITEM n;                       /* number of items */
              
              assert(t);                    /* check the function argument */
              if ((n = t->size) < 2) return;/* do not reverse less than two items */
              while ((n > 0) && (t->items[n-1] <= TA_END))
                --n;                        /* skip additional end markers and */
              ia_reverse(t->items, (size_t)n);  /* reverse the remaining items */
              }  /* ta_reverse() */
              
              /*--------------------------------------------------------------------*/
              
              ITEM ta_unique (TRACT *t)
              {                               /* --- make items unique */
              ITEM k, n;                    /* number of items */
              
              assert(t);                    /* check the function argument */
              if ((n = t->size) < 2)        /* do not process less than two items */
              return n;                   /* (cannot contain duplicates) */
              while ((n > 0) && (t->items[n-1] <= TA_END))
                --n;                        /* skip additional end markers and */
              k = (ITEM)ia_unique(t->items, (size_t)n);   /* remove duplicates */
              t->size -= n-k;               /* adapt the transaction size */
              while (k < t->size)           /* if necessary, fill again */
              t->items[k++] = TA_END;     /* with additional end markers */
              return t->size;               /* return the new number of items */
              }  /* ta_unique() */
              
              /*--------------------------------------------------------------------*/
              
              ITEM ta_pack (TRACT *t, int n)
              {                               /* --- pack items with codes 0--(n-1) */
              ITEM b;                       /* packed items (bit array) */
              ITEM *s, *d, *p;              /* to traverse the items */
              
              assert(t);                    /* check the function arguments */
              if (n <= 0) return 0;         /* if no items to pack, abort */
              if (n > 31) n = 31;           /* pack at most 31 items */
              for (s = t->items; *s > TA_END; s++)
                if (*s < n) break;          /* find item with code < n */
              if (*s <= TA_END) return 0;   /* if there is none, abort */
              p = d = s;                    /* note the destination location */
              for (b = 0; *s > TA_END; s++) {
                if      (*s < 0) b |= *s;   /* traverse the remaining items */
              else if (*s < n) b |= 1 << *s;
              else             *++d = *s; /* set bits for items with codes < n */
              }                             /* and copy the other items */
              *p = b | TA_END;              /* store the packed items (bit rep.) */
              while (++d < s) *d = TA_END;  /* clear rest of the transaction */
              return b & ~TA_END;           /* return bit rep. of packed items */
              }  /* ta_pack() */
              
              /*--------------------------------------------------------------------*/
              
              ITEM ta_unpack (TRACT *t, int dir)
              {                               /* --- unpack items (undo ta_pack()) */
              ITEM p, q, i, k;              /* packed items, loop variables */
              ITEM *s, *d;                  /* to traverse the items */
              
              assert(t);                    /* check the function arguments */
              for (d = t->items; *d >= 0; d++);  /* search for packed items */
              if (*d <= TA_END) return 0;   /* if there are none, abort */
              for (i = k = 0, q = p = *d & ~TA_END; q; q >>= 1) {
                i += q & 1; k++; }          /* get and count the packed items */
              for (s = d+1; *s > TA_END; s++);    /* find end of transaction */
              memmove(d+i, d+1, (size_t)(s-d) *sizeof(ITEM));   /* move rest */
              if (dir < 0) {                /* if negative direction requested, */
              for (i = k; --i >= 0; )     /* store items in descending order */
              if (p & (1 << i)) *d++ = i; }
              else {                        /* if positive direction requested, */
              for (i = 0; i < k; i++)     /* store items in ascending order */
              if (p & (1 << i)) *d++ = i; }
              return p & ~TA_END;           /* return bit rep. of packed items */
              }  /* ta_unpack() */
              
              /*--------------------------------------------------------------------*/
              
              int ta_equal (const TRACT *t1, const TRACT *t2)
              {                               /* --- compare transactions */
              const ITEM *a, *b;            /* to traverse the items */
              
              assert(t1 && t2);             /* check the function arguments */
              if (t1->size != t2->size)     /* if the sizes differ, */
              return -1;                  /* transactions cannot be equal */
              for (a = t1->items, b = t2->items; *a > TA_END; a++, b++)
                if (*a != *b) return -1;    /* abort if corresp. items differ */
              return 0;                     /* return that trans. are equal */
              }  /* ta_equal() */
              
              /*--------------------------------------------------------------------*/
              
              int ta_cmp (const void *p1, const void *p2, void *data)
              {                               /* --- compare transactions */
              const ITEM *a, *b;            /* to traverse the items */
              
              assert(p1 && p2);             /* check the function arguments */
              a = ((const TRACT*)p1)->items;
              b = ((const TRACT*)p2)->items;
              for ( ; 1; a++, b++) {        /* traverse the item arrays */
              if (*a < *b) return -1;     /* compare corresponding items */
              if (*a > *b) return +1;     /* and if one is greater, abort */
              if (*a <= TA_END) return 0; /* otherwise check for the sentinel */
              }                             /* and abort if it is reached */
              }  /* ta_cmp() */
              
              /* Note that this comparison function also works correctly if there */
              /* are packed items, since packed item entries are always > TA_END. */
              
              /*--------------------------------------------------------------------*/
              
              int ta_cmpep (const void *p1, const void *p2, void *data)
              {                               /* --- compare trans. (equal packed) */
              ITEM       i, k;              /* item buffers */
              const ITEM *a, *b;            /* to traverse the items */
              
              assert(p1 && p2);             /* check the function arguments */
              a = ((const TRACT*)p1)->items;
              b = ((const TRACT*)p2)->items;
              for ( ; 1; a++, b++) {        /* traverse the item arrays */
              i = (*a >= 0) ? *a : 0;     /* get the next items, but */
              k = (*b >= 0) ? *b : 0;     /* use 0 if items are packed */
              if (i < k) return -1;       /* compare corresponding items */
              if (i > k) return +1;       /* and if one is greater, abort */
              if (*a <= TA_END) return 0; /* otherwise check for the sentinel */
              }                             /* and abort if it is reached */
              }  /* ta_cmpep() */
              
              /*--------------------------------------------------------------------*/
              
              int ta_cmpoff (const void *p1, const void *p2, void *data)
              {                               /* --- compare transactions */
              ITEM i, k;                    /* item buffers */
              
              assert(p1 && p2);             /* check the function arguments */
              i = ((const TRACT*)p1)->items[*(ITEM*)data];
              k = ((const TRACT*)p2)->items[*(ITEM*)data];
              if (i < k) return -1;         /* compare items at given offset */
              if (i > k) return +1;         /* and if one is greater, abort */
              return 0;                     /* otherwise return 'equal' */
              }  /* ta_cmpoff() */
              
              /*--------------------------------------------------------------------*/
              
              int ta_cmplim (const void *p1, const void *p2, void *data)
              {                               /* --- compare transactions limited */
              const ITEM *a, *b;            /* to traverse the items */
              
              assert(p1 && p2);             /* check the function arguments */
              a = ((const TRACT*)p1)->items;
              b = ((const TRACT*)p2)->items;
              for ( ; 1; a++, b++) {        /* traverse the item arrays */
              if (*a < *b) return -1;     /* compare corresponding items */
              if (*a > *b) return +1;     /* and if one is greater, abort */
              if (*a == *(ITEM*)data) return 0;
              }                             /* abort if the limit is reached */
              }  /* ta_cmplim() */
              
              /*--------------------------------------------------------------------*/
              
              int ta_cmpsfx (const void *p1, const void *p2, void *data)
              {                               /* --- compare transaction suffixes */
              const ITEM *a, *b;            /* to traverse the items */
              
              assert(p1 && p2);             /* check the function arguments */
              a = ((const TRACT*)p1)->items +*(ITEM*)data;
              b = ((const TRACT*)p2)->items +*(ITEM*)data;
              for ( ; 1; a++, b++) {        /* traverse the item arrays */
              if (*a < *b) return -1;     /* compare corresponding items */
              if (*a > *b) return +1;     /* and if one is greater, abort */
              if (*a <= TA_END) return 0; /* otherwise check for the sentinel */
              }                             /* and abort if it is reached */
              }  /* ta_cmpsfx() */
              
              /*--------------------------------------------------------------------*/
              
              int ta_cmpsep (const void *p1, const void *p2, void *data)
              {                               /* --- compare transaction suffixes */
              ITEM       i, k;              /* item buffers */
              const ITEM *a, *b;            /* to traverse the items */
              
              assert(p1 && p2);             /* check the function arguments */
              a = ((const TRACT*)p1)->items +*(ITEM*)data;
              b = ((const TRACT*)p2)->items +*(ITEM*)data;
              for ( ; 1; a++, b++) {        /* traverse the item arrays */
              i = ((*a >= 0) || (*a <= TA_END)) ? *a : 0; /* use 0 for */
              k = ((*b >= 0) || (*b <= TA_END)) ? *b : 0; /* packed items */
              /* Note that the item identifier 0 cannot occur if items have */
              /* been packed, because this identifier must then be packed.  */
              if (i < k) return -1;       /* compare corresponding items */
              if (i > k) return +1;       /* and if one is greater, abort */
              if (*a <= TA_END) return 0; /* otherwise check for the sentinel */
              }                             /* and abort if it is reached */
              }  /* ta_cmpsep() */
              
              /*--------------------------------------------------------------------*/
              
              int ta_cmpx (const TRACT *t, const ITEM *items, ITEM n)
              {                               /* --- compare transactions */
              const ITEM *i, *end;          /* to traverse the items */
              
              assert(t && items);           /* check the function arguments */
              end = items +((n < t->size) ? n : t->size);
              for (i = t->items; items < end; i++, items++) {
                if (*i < *items) return -1; /* compare corresponding items */
              if (*i > *items) return +1; /* and abort the comparison */
              }                             /* if one of them is greater */
              return (t->size < n) ? -1 : (t->size > n) ? +1 : 0;
              }  /* ta_cmpx() */              /* return sign of size difference */
              
              /*--------------------------------------------------------------------*/
              
              int ta_cmpsz (const void *p1, const void *p2, void *data)
              {                               /* --- compare transactions */
              ITEM a, b;                    /* transaction sizes */
              
              assert(p1 && p2);             /* check the function arguments */
              a = ((const TRACT*)p1)->size; /* return the sign */
              b = ((const TRACT*)p2)->size; /* of the size difference */
              return (a > b) ? 1 : (a < b) ? -1 : ta_cmp(p1, p2, data);
              }  /* ta_cmpsz() */
              
              /*--------------------------------------------------------------------*/
              
              ITEM ta_subset (const TRACT *t1, const TRACT *t2, ITEM off)
              {                               /* --- test for subset/subsequence */
              const ITEM *s, *d, *x, *y;    /* to traverse the items */
              
              assert(t1 && t2 && (off >= 0));  /* check the function arguments */
              if ((off > t2->size) || (t1->size > t2->size -off))
                return -1;                  /* check for (enough) items in dest. */
              s = t1->items;                /* check for empty source sequence, */
              if (*s <= TA_END) return 0;   /* then traverse the destination */
              for (d = t2->items +off; *d > TA_END; d++) {
                if (*d != *s) continue;     /* try to find source start in dest. */
              for (x = s+1, y = d+1; 1; y++) {  /* compare the remaining items */
              if (*x <= TA_END) return (ITEM)(d -t2->items);
              if (*y <= TA_END) break;  /* all in source matched, subset, */
              if (*x == *y) x++;        /* end of destination, no subset */
              }                           /* skip matched item in source and */
              }                             /* always skip item in destination */
              return -1;                    /* return 'not subsequence w/o gaps' */
              }  /* ta_subset() */
              
              /*--------------------------------------------------------------------*/
              
              ITEM ta_subwog (const TRACT *t1, const TRACT *t2, ITEM off)
              {                               /* --- test for subsequence w/o gaps */
              const ITEM *s, *d, *x, *y;    /* to traverse the segments */
              
              assert(t1 && t2 && (off >= 0));  /* check the function arguments */
              if ((off > t2->size) || (t1->size > t2->size -off))
                return -1;                  /* check for (enough) items in dest. */
              s = t1->items;                /* check for empty source sequence, */
              if (*s <= TA_END) return 0;   /* then traverse the destination */
              for (d = t2->items +off; *d > TA_END; d++) {
                if (*d != *s) continue;     /* try to find source start in dest. */
              x = s; y = d;               /* compare the remaining items */
              do { if (*++x <= TA_END) return (ITEM)(d -t2->items); }
              while (*x == *++y);         /* if all items have been found, */
              }                             /* abort with success, otherwise */
              return -1;                    /* return 'not subsequence w/o gaps' */
              }  /* ta_subwog() */
              
              /*----------------------------------------------------------------------
              Weighted Item Functions
              ----------------------------------------------------------------------*/
              
              int wi_cmp (const WITEM *a, const WITEM *b)
              {                               /* --- compare two transactions */
              ITEM i;                       /* loop variable */
              
              assert(a && b);               /* check the function arguments */
              for (i = 0; 1; i++) {         /* compare the items */
              if (a[i].item > b[i].item) return +1;
              if (a[i].item < b[i].item) return -1;
              if (a[i].item < 0) break;   /* check for the sentinel and */
              }                             /* abort if it is reached */
              for (i = 0; 1; i++) {         /* compare the item weights */
              if (a[i].wgt  > b[i].wgt) return +1;
              if (a[i].wgt  < b[i].wgt) return -1;
              if (a[i].item < 0) return 0;/* check for the sentinel and */
              }                             /* abort if it is reached */
              }  /* wi_cmp() */
              
              /*--------------------------------------------------------------------*/
              
              static void wi_rec (WITEM *wia, ITEM n)
              {                               /* --- recursive part of item sorting */
              WITEM *l, *r, t;              /* exchange positions and buffer */
              ITEM  x, m;                   /* pivot element, number of elements */
              
              do {                          /* sections sort loop */
              l = wia; r = l +n -1;       /* start at left and right boundary */
              if (l->item > r->item) {    /* bring the first and last element */
              t = *l; *l = *r; *r = t;} /* into the proper order */
              x = wia[n >> 1].item;       /* get the middle element as pivot */
              if      (x < l->item) x = l->item;  /* try to find a */
              else if (x > r->item) x = r->item;  /* better pivot element */
              while (1) {                 /* split and exchange loop */
              while ((++l)->item < x);  /* skip smaller elements on the left */
              while ((--r)->item > x);  /* skip greater elements on the right */
              if (l >= r) {             /* if less than two elements left, */
              if (l <= r) { l++; r--; } break; }       /* abort the loop */
              t = *l; *l = *r; *r = t;  /* otherwise exchange elements */
              }
              m = (ITEM)(wia +n -l);      /* compute the number of elements */
              n = (ITEM)(r -wia +1);      /* right and left of the split */
              if (n > m) {                /* if right section is smaller, */
              if (m >= TH_INSERT)       /* but larger than the threshold, */
              wi_rec(l, m); }         /* sort it by a recursive call, */
              else {                      /* if the left section is smaller, */
              if (n >= TH_INSERT)       /* but larger than the threshold, */
              wi_rec(wia, n);         /* sort it by a recursive call, */
              wia = l; n = m;           /* then switch to the right section */
              }                           /* keeping its size m in variable n */
              } while (n >= TH_INSERT);     /* while greater than threshold */
              }  /* wi_rec() */
              
              /*--------------------------------------------------------------------*/
              
              void wi_sort (WITEM *wia, ITEM n, int dir)
              {                               /* --- sort an transaction item array */
              ITEM  i, k;                   /* loop variable, first section */
              WITEM *l, *r;                 /* to traverse the array */
              WITEM t;                      /* exchange buffer */
              
              assert(wia && (n >= 0));      /* check the function arguments */
              if (n <= 1) return;           /* do not sort less than two elements */
              if (n < TH_INSERT)            /* if fewer elements than threshold */
              k = n;                      /* for insertion sort, note the */
              else {                        /* number of elements, otherwise */
              wi_rec(wia, n);             /* call the recursive function */
              k = TH_INSERT -1;           /* and get the number of elements */
              }                             /* in the first array section */
              for (l = r = wia; --k > 0; )  /* find the smallest element */
              if ((++r)->item < l->item)  /* among the first k elements */
              l = r;                    /* to use as a front sentinel */
              r = wia;                      /* swap the smallest element */
              t = *l; *l = *r; *r = t;      /* to the front as a sentinel */
              for (i = n; --i > 0; ) {      /* insertion sort loop */
              t = *++r;                   /* note the element to insert */
              for (l = r; (--l)->item > t.item; )  /* shift right elements */
              l[1] = *l;                /* that are greater than the one */
              l[1] = t;                   /* to insert and store this element */
              }                             /* in the place thus found */
              if (dir < 0)                  /* if descending order requested, */
              wi_reverse(wia, n);         /* reverse the element order */
              }  /* wi_sort() */
              
              /*--------------------------------------------------------------------*/
              
              void wi_reverse (WITEM *wia, ITEM n)
              {                               /* --- reverse an item array */
              WITEM t, *end = wia +n;       /* exchange buffer, end pointer */
              
              assert(wia && (n >= 0));      /* check the function arguments */
              while (--end > wia) {         /* reverse the order of the items */
              t = *end; *end = *wia; *wia++ = t; }
              }  /* wi_reverse() */
              
              /*--------------------------------------------------------------------*/
              
              ITEM wi_unique (WITEM *wia, ITEM n)
              {                               /* --- make item array unique */
              WITEM *s, *d;                 /* to traverse the item array */
              
              assert(wia && (n >= 0));      /* check the function arguments */
              if (n <= 1) return n;         /* check for 0 or 1 element */
              for (d = s = wia; --n > 0; ) {
                if      ((++s)->item != d->item) *++d = *s;
                else if (   s->wgt > d->wgt) d->wgt = s->wgt;
              }                             /* collect the unique elements */
              *++d = WTA_END;               /* store a sentinel at the end */
              return (ITEM)(d -wia);        /* return the new number of elements */
              }  /* wi_unique() */
              
              /*----------------------------------------------------------------------
              Transaction Functions (weighted items)
              ----------------------------------------------------------------------*/
              
              WTRACT* wta_clone (const WTRACT *t)
              {                               /* --- clone a transaction */
              WTRACT *c;                    /* created transaction clone */
              
              assert(t);                    /* check the function argument */
              c = (WTRACT*)malloc(sizeof(WTRACT) +(size_t)t->size *sizeof(WITEM));
              if (!c) return NULL;          /* allocate a new transaction and */
              c->wgt  = t->wgt;             /* copy old transaction into it */
              c->size = t->size; c->mark = t->mark;
              memcpy(c->items, t->items, (size_t)t->size *sizeof(WITEM));
              c->items[t->size] = WTA_END;  /* store a sentinel at the end */
              return c;                     /* return the created copy */
              }  /* wta_clone() */
              
              /*--------------------------------------------------------------------*/
              
              WTRACT* wta_copy (WTRACT *dst, const WTRACT *src)
              {                               /* --- copy a transaction */
              assert(dst                    /* check the function arguments */
              &&     src && (dst->size == src->size));
                dst->wgt = src->wgt;          /* copy weight and items */
              memcpy(dst->items, src->items, (size_t)(src->size+1) *sizeof(WITEM));
              return dst;                   /* return the destination */
              }  /* ta_copy() */
              /*--------------------------------------------------------------------*/
              
              int wta_cmp (const void *p1, const void *p2, void *data)
              {                               /* --- compare transactions */
              return wi_cmp(((const WTRACT*)p1)->items,
                            ((const WTRACT*)p2)->items);
              }  /* wta_cmp() */
              
              /*----------------------------------------------------------------------
              Transaction Bag/Multiset Functions
              ----------------------------------------------------------------------*/
              
              TABAG* tbg_create (ITEMBASE *base)
              {                               /* --- create a transaction bag */
              TABAG *bag;                   /* created transaction bag */
              
              assert(base);                 /* check the function argument */
              bag = (TABAG*)malloc(sizeof(TABAG));
              if (!bag) return NULL;        /* create a transaction bag/multiset */
              if (!base && !(base = ib_create(0, 0))) {
                free(bag); return NULL; }   /* create an item base if needed */
              bag->base   = base;           /* store the underlying item base */
              bag->mode   = base->mode;     /* and initialize the other fields */
              bag->extent = 0; bag->wgt = 0; bag->max = 0;
              bag->cnt    = bag->size = 0;
              bag->tracts = NULL;           /* there are no transactions yet */
              bag->icnts  = NULL;
              bag->ifrqs  = NULL;
              bag->buf    = NULL;
              return bag;                   /* return the created t.a. bag */
              }  /* tbg_create() */
              
              /*--------------------------------------------------------------------*/
              
              void tbg_delete (TABAG *bag, int delib)
              {                               /* --- delete a transaction bag */
              assert(bag);                  /* check the function argument */
              if (bag->buf) free(bag->buf); /* delete buffer for surrogates */
              if (bag->tracts) {            /* if there are transactions */
              while (bag->cnt > 0)        /* traverse the transaction array */
              free(bag->tracts[--bag->cnt]);
                free(bag->tracts);          /* delete all transactions */
              }                             /* and the transaction array */
              if (bag->icnts) free(bag->icnts);
              if (delib) ib_delete(bag->base);
              free(bag);                    /* delete the item base and */
              }  /* tbg_delete() */           /* the transaction bag body */
              
              /*--------------------------------------------------------------------*/
              
              static TABAG* clone (TABAG *bag)
              {                               /* --- clone memory structure */
              TID    i;                     /* loop variable */
              ITEM   n;                     /* number of items */
              TABAG  *dst;                  /* created clone of the trans. bag */
              TRACT  *t;                    /* to traverse the transactions */
              WTRACT *x;                    /* to traverse the transactions */
              
              assert(bag);                  /* check the function argument */
              dst = tbg_create(bag->base);  /* create an empty transaction bag */
              dst->tracts = (void**)malloc((size_t)bag->cnt *sizeof(TRACT*));
              if (!dst->tracts) { return NULL; }
              dst->max    = bag->max;       /* create a transaction array */
              dst->wgt    = bag->wgt;       /* copy maximum transaction size, */
              dst->extent = bag->extent;    /* total transaction weight and */
              dst->size   = bag->cnt;       /* transaction array size and extent */
              if (bag->mode & IB_WEIGHTS) { /* if trans. with weighted items */
              for (i = 0; i < bag->cnt; i++) { /* traverse the transactions */
              n = ((WTRACT*)bag->tracts[i])->size;
                x = (WTRACT*)malloc(sizeof(TRACT) +(size_t)(n+1) *sizeof(ITEM));
                if (!x) { tbg_delete(dst, 0); return NULL; }
                x->wgt  = 1;              /* create and init. the transaction */
              x->size = n; x->mark = 0; x->items[n] = WTA_END;
              dst->tracts[dst->cnt++] = x;
              } }                         /* store the created transaction */
              else {                        /* if simple transactions */
              for (i = 0; i < bag->cnt; i++) { /* traverse the transactions */
              n = ((TRACT*)bag->tracts[i])->size;
                t = (TRACT*)malloc(sizeof(TRACT) +(size_t)(n+1) *sizeof(ITEM));
                if (!t) { tbg_delete(dst, 0); return NULL; }
                t->wgt  = 1;              /* create and init. the transaction */
              t->size = n; t->mark = 0; t->items[n] = TA_END;
              dst->tracts[dst->cnt++] = t;
              }                           /* note the transaction size and */
              }                             /* store the created transaction */
              return dst;                   /* return the created clone */
              }  /* clone() */
              
              /*--------------------------------------------------------------------*/
              
              TABAG* tbg_clone (TABAG *bag)
              {                               /* --- clone a transaction bag */
              TABAG *dst = clone(bag);      /* clone the memory structure */
              if (dst) tbg_copy(dst, bag);  /* copy the transactions into it */
              return dst;                   /* return the created clone */
              }  /* tbg_clone() */
              
              /*--------------------------------------------------------------------*/
              
              TABAG* tbg_copy (TABAG *dst, TABAG *src)
              {                               /* --- copy a transaction bag */
              TID i;                        /* loop variable */
              
              assert(dst && src             /* check the function arguments */
              &&    (dst->size >= src->cnt));
              if (src->mode & IB_WEIGHTS) { /* if trans. with weighted items */
              for (i = 0; i < src->cnt; i++)  /* copy the transactions */
              wta_copy(dst->tracts[i], src->tracts[i]); }
              else {                        /* if simple transactions */
              for (i = 0; i < src->cnt; i++)  /* copy the transactions */
              ta_copy(dst->tracts[i], src->tracts[i]);
              }
              dst->mode = src->mode;        /* copy the transaction mode */
              return dst;                   /* return the destination bag */
              }  /* tbg_copy() */
              
              /*--------------------------------------------------------------------*/
              
              static int tbg_count (TABAG *bag)
              {                               /* --- count item occurrences */
              ITEM   i;                     /* item buffer, number of items */
              TID    n;                     /* loop variable for transactions */
              TRACT  *t;                    /* to traverse the transactions */
              WTRACT *x;                    /* to traverse the transactions */
              ITEM   *s;                    /* to traverse the transaction items */
              WITEM  *p;                    /* to traverse the transaction items */
              TID    *z;                    /* to reallocate counter arrays */
              
              i = ib_cnt(bag->base);        /* get the number of items */
              z = (TID*)realloc(bag->icnts, (size_t)i *(sizeof(TID)+sizeof(SUPP)));
              if (!z) return -1;            /* allocate the counter arrays */
              bag->icnts = (TID*) memset(z,            0, (size_t)i *sizeof(TID));
              bag->ifrqs = (SUPP*)memset(bag->icnts+i, 0, (size_t)i *sizeof(SUPP));
              if (bag->mode & IB_WEIGHTS) { /* if the items carry weights */
              for (n = 0; n < bag->cnt; n++) {
                x = (WTRACT*)bag->tracts[n]; /* traverse the transactions */
              for (p = x->items; p->item >= 0; p++) {
                bag->icnts[p->item] += 1;  /* traverse the transaction items */
              bag->ifrqs[p->item] += x->wgt;
              }                         /* count the occurrences and */
              } }                         /* sum the transaction weights */
              else {                        /* if the items do not carry weights */
              for (n = 0; n < bag->cnt; n++) {
                t = (TRACT*)bag->tracts[n];  /* traverse the transactions */
              for (s = t->items; *s > TA_END; s++) {
                if ((i = *s) < 0) i = 0;   /* traverse the transaction items */
              bag->icnts[i] += 1;     /* count packed items in 1st element */
              bag->ifrqs[i] += t->wgt;
              }                         /* count the occurrences and */
              }                           /* sum the transaction weights */
              }
              return 0;                     /* return 'ok' */
              }  /* tbg_count() */
              
              /*--------------------------------------------------------------------*/
              
              const TID* tbg_icnts (TABAG *bag, int recnt)
              {                               /* --- number of trans. per item */
              if ((recnt || !bag->icnts) && (tbg_count(bag) < 0)) return NULL;
              return bag->icnts;            /* count the item occurrences */
              }  /* tbg_icnts() */
              
              /*--------------------------------------------------------------------*/
              
              const SUPP* tbg_ifrqs (TABAG *bag, int recnt)
              {                               /* --- item frequencies (weight) */
              if ((recnt || !bag->ifrqs) && (tbg_count(bag) < 0)) return NULL;
              return bag->ifrqs;            /* determine the item frequencies */
              }  /* tbg_ifrqs() */
              
              /*--------------------------------------------------------------------*/
              
              int tbg_add (TABAG *bag, TRACT *t)
              {                               /* --- add a standard transaction */
              void **p;                     /* new transaction array */
              TID  n;                       /* new transaction array size */
              
              assert(bag                    /* check the function arguments */
              &&   !(bag->mode & IB_WEIGHTS));
              n = bag->size;                /* get the transaction array size */
              if (bag->cnt >= n) {          /* if the transaction array is full */
              n += (n > BLKSIZE) ? (n >> 1) : BLKSIZE;
                p  = (void**)realloc(bag->tracts, (size_t)n *sizeof(TRACT*));
                if (!p) return E_NOMEM;     /* enlarge the transaction array */
              bag->tracts = p; bag->size = n;
              }                             /* set the new array and its size */
              if (!t && !(t = ta_clone(ib_tract(bag->base))))
                return E_NOMEM;             /* get trans. from item base if nec. */
              if (bag->icnts) {             /* delete the item-specific counters */
              free(bag->icnts); bag->icnts = NULL; bag->ifrqs = NULL; }
              bag->tracts[bag->cnt++] = t;  /* store the transaction and */
              bag->wgt += t->wgt;           /* sum the transaction weight */
              if (t->size > bag->max)       /* update maximal transaction size */
              bag->max = t->size;         /* and count the item instances */
              bag->extent += (size_t)t->size;
              return 0;                     /* return 'ok' */
              }  /* tbg_add() */
              
              /*--------------------------------------------------------------------*/
              
              int tbg_addw (TABAG *bag, WTRACT *t)
              {                               /* --- add an extended transaction */
              void **p;                     /* new transaction array */
              TID  n;                       /* new transaction array size */
              
              assert(bag                    /* check the function arguments */
              &&    (bag->mode & IB_WEIGHTS));
              n = bag->size;                /* get the transaction array size */
              if (bag->cnt >= n) {          /* if the transaction array is full */
              n += (n > BLKSIZE) ? (n >> 1) : BLKSIZE;
                p  = (void**)realloc(bag->tracts, (size_t)n *sizeof(WTRACT*));
                if (!p) return E_NOMEM;     /* enlarge the transaction array */
              bag->tracts = p; bag->size = n;
              }                             /* set the new array and its size */
              if (!t && !(t = wta_clone(ib_wtract(bag->base))))
                return E_NOMEM;             /* get trans. from item base if nec. */
              if (bag->icnts) {             /* delete the item-specific counters */
              free(bag->icnts); bag->icnts = NULL; bag->ifrqs = NULL; }
              bag->tracts[bag->cnt++] = t;  /* store the transaction and */
              bag->wgt += t->wgt;           /* sum the transaction weight */
              if (t->size > bag->max)       /* update maximal transaction size */
              bag->max = t->size;         /* and count the item instances */
              bag->extent += (size_t)t->size;
              return 0;                     /* return 'ok' */
              }  /* tbg_addw() */
              
              /*--------------------------------------------------------------------*/
              
              int tbg_addib (TABAG *bag)
              {                               /* --- add transaction from item base */
              assert(bag);                  /* check the function argument */
              return (bag->mode & IB_WEIGHTS) ? tbg_addw(bag, NULL)
                : tbg_add (bag, NULL);
              }  /* tbg_addib() */
              
              int tbg_istab (TABAG *bag)
              {                               /* --- check for table-derived data */
              int      r = -1;              /* result of check for table */
              ITEM     i, n, z;             /* loop variable for items */
              TID      k;                   /* loop variable for transactions */
              TRACT    *t;                  /* to traverse the transactions */
              ITEMDATA *itd;                /* to traverse the item data */
              IDMAP    *idmap;              /* item identifier map */
              
              assert(bag                    /* check the function arguments */
              &&   ((bag->mode & TA_PACKED) == 0));
              if (bag->cnt <= 1) return 0;  /* check for at most one transaction */
              idmap = bag->base->idmap;     /* get the item identifier map */
              n     = idm_cnt(idmap);       /* get the number of items and */
              for (i = n; --i >= 0; )       /* clear the occurrence columns */
              ((ITEMDATA*)idm_byid(idmap, i))->idx = -1;
              z = ((TRACT*)bag->tracts[0])->size;   /* get transaction size */
              for (k = bag->cnt; --k >= 0; ) {
                t = bag->tracts[k];         /* traverse the transactions */
              if (t->size != z) {         /* check the size of the transactions */
              r = 0; break; }           /* (must all have the same size) */
              for (i = t->size; --i >= 0; ) { /* traverse the items */
              itd = (ITEMDATA*)idm_byid(idmap, t->items[i]);
                if      (itd->idx <  0)      itd->idx = (TID)i;
                else if (itd->idx != (TID)i) { r = 0; break; }
              }                           /* check whether all items always */
              }                             /* occur in the same column */
              bag->base->idx = 1;           /* reset the global marker/index */
              for (i = n; --i >= 0; )       /* and the item markers/indices */
              ((ITEMDATA*)idm_byid(idmap, i))->idx = 0;
              return r;                     /* return the test result */
              }  /* tbg_istab() */
              
              /*--------------------------------------------------------------------*/
              
              static void recode (TABAG *bag, ITEM *map)
              {                               /* --- recode items in transactions */
              ITEM   i;                     /* item buffer */
              TID    n;                     /* loop variable for transactions */
              TRACT  *t;                    /* to traverse the transactions */
              WTRACT *x;                    /* to traverse the transactions */
              ITEM   *s, *d;                /* to traverse the items */
              WITEM  *a, *b;                /* to traverse the items */
              
              assert(bag && map);           /* check the function arguments */
              if (bag->icnts) {             /* delete the item-specific counters */
              free(bag->icnts); bag->icnts = NULL; bag->ifrqs = NULL; }
              bag->extent = 0; bag->max = 0;/* clear maximal transaction size */
              if (bag->mode & IB_WEIGHTS) { /* if the items carry weights */
              for (n = 0; n < bag->cnt; n++) {
                x = (WTRACT*)bag->tracts[n];  /* traverse the transactions */
              for (a = b = x->items; a->item >= 0; a++) {
                i = map[a->item];       /* traverse and recode the items */
              if (i >= 0) (b++)->item = i;    /* remove all items that are */
              }                             /* not mapped (mapped to id < 0) */
              x->size = (ITEM)(b-x->items); /* compute new number of items */
              x->items[x->size] = WTA_END;  /* store a sentinel at the end */
              if (x->size > bag->max)   /* update the maximal trans. size */
              bag->max = x->size;     /* (may differ from the old size) */
              bag->extent += (size_t)x->size;
              } }                         /* sum the item instances */
              else {                        /* if the items do not carry weights */
              for (n = 0; n < bag->cnt; n++) {
                t = (TRACT*)bag->tracts[n];   /* traverse the transactions */
              for (s = d = t->items; *s > TA_END; s++) {
                i = map[*s];            /* traverse and recode the items */
              if (i >= 0) *d++ = i;   /* remove all items that are */
              }                         /* not mapped (mapped to id < 0) */
              t->size = (ITEM)(d-t->items); /* compute new number of items */
              t->items[t->size] = TA_END;   /* store a sentinel at the end */
              if (t->size > bag->max)   /* update the maximal trans. size */
              bag->max = t->size;     /* (may differ from the old size) */
              bag->extent += (size_t)t->size;
              }                           /* sum the item instances */
              }
              }  /* recode() */
              
              /*--------------------------------------------------------------------*/
              
              ITEM tbg_recode (TABAG *bag, SUPP min, SUPP max, ITEM cnt, int dir)
              {                               /* --- recode items in transactions */
              ITEM *map;                    /* identifier map for recoding */
              
              assert(bag);                  /* check the function arguments */
              map = (ITEM*)malloc((size_t)ib_cnt(bag->base) *sizeof(ITEM));
              if (!map) return -1;          /* create an item identifier map */
              cnt = ib_recode(bag->base, min, max, cnt, dir, map);
              recode(bag, map);             /* recode items and transactions */
              free(map);                    /* delete the item identifier map */
              return cnt;                   /* return the new number of items */
              }  /* tbg_recode() */
              
              /*--------------------------------------------------------------------*/
              
              void tbg_filter (TABAG *bag, ITEM min, const int *marks, double wgt)
              {                               /* --- filter (items in) transactions */
              TID    n;                     /* loop variable for transactions */
              TRACT  *t;                    /* to traverse the transactions */
              WTRACT *x;                    /* to traverse the transactions */
              ITEM   *s, *d;                /* to traverse the items */
              WITEM  *a, *b;                /* to traverse the items */
              
              assert(bag);                  /* check the function arguments */
              if (!marks && (min <= 1)) return;
              if (bag->icnts) {             /* delete the item-specific counters */
              free(bag->icnts); bag->icnts = NULL; bag->ifrqs = NULL; }
              bag->extent = 0;              /* clear the item instance counter */
              bag->max    = 0;              /* and the maximal transaction size */
              if (bag->mode & IB_WEIGHTS) { /* if the items carry weights */
              for (n = 0; n < bag->cnt; n++) {
                x = (WTRACT*)bag->tracts[n]; /* traverse the transactions */
              if (marks) {              /* if item markers are given */
              for (a = b = x->items; a->item >= 0; a++)
                if (marks[a->item] && (a->wgt >= wgt))
                  *b++ = *a;          /* remove unmarked items */
              x->size = (ITEM)(b -x->items);
              }                         /* store the new number of items */
              if (x->size < min)        /* if the transaction is too short, */
              x->size = 0;            /* delete all items (clear size) */
              x->items[x->size] = WTA_END; /* store a sentinel at the end */
              if (x->size > bag->max)   /* update the maximal trans. size */
              bag->max = x->size;     /* (may differ from the old size) */
              bag->extent += (size_t)x->size;
              } }                         /* sum the item instances */
              else {                        /* if the items do not carry weights */
              for (n = 0; n < bag->cnt; n++) {
                t = (TRACT*)bag->tracts[n];  /* traverse the transactions */
              if (marks) {              /* if item markers are given */
              for (s = d = t->items; *s > TA_END; s++)
                if (marks[*s]) *d++ = *s; /* remove unmarked items */
              t->size = (ITEM)(d -t->items);
              }                         /* store the new number of items */
              if (t->size < min)        /* if the transaction is too short, */
              t->size = 0;            /* delete all items (clear size) */
              t->items[t->size] = TA_END;  /* store a sentinel at the end */
              if (t->size > bag->max)   /* update the maximal trans. size */
              bag->max = t->size;     /* (may differ from the old size) */
              bag->extent += (size_t)t->size;
              }                           /* sum the item instances */
              }
              }  /* tbg_filter() */
              
              
              void tbg_itsort (TABAG *bag, int dir, int heap)
              {                               /* --- sort items in transactions */
              ITEM   k;                     /* number of items */
              TID    n;                     /* loop variable */
              TRACT  *t;                    /* to traverse the transactions */
              WTRACT *x;                    /* to traverse the transactions */
              void   (*sortfn)(ITEM*, size_t, int);  /* sort function */
              
              assert(bag);                  /* check the function arguments */
              if (bag->mode & IB_WEIGHTS) { /* if the items carry weights */
              for (n = 0; n < bag->cnt; n++) {
                x = (WTRACT*)bag->tracts[n]; /* traverse the transactions */
              wi_sort(x->items, x->size, dir);
              } }                         /* sort the items in each transaction */
              else {                        /* if the items do not carry weights */
              sortfn = (heap) ? ia_heapsort : ia_qsort;
                for (n = 0; n < bag->cnt; n++) {
                  t = (TRACT*)bag->tracts[n];  /* traverse the transactions */
              k = t->size;              /* get transaction and its size */
              if (k < 2) continue;      /* do not sort less than two items */
              while ((k > 0) && (t->items[k-1] <= TA_END))
                --k;                    /* skip additional end markers */
              sortfn(t->items, (size_t)k, dir);
                }                           /* sort the items in the transaction */
              }                             /* if the given direction is negative */
              }  /* tbg_itsort() */
              
              /*--------------------------------------------------------------------*/
              
              static void pksort (TRACT **tracts, TRACT **buf, TID n, ITEM o)
              {                               /* --- sort packed items with binsort */
              ITEM  i, k, x;                /* loop variables, bit pattern */
              ITEM  mask;                   /* overall item bit mask */
              TRACT **dst, **src, **t;      /* to traverse the transactions */
              TID   cnts[64];               /* counters for bit pattern occs. */
              
              assert(tracts && buf);        /* check the function arguments */
              if (n <= 1) return;           /* sort only 2 and more transactions */
              if (n <= 32) {                /* sort few transactions plainly */
              ptr_mrgsort(tracts, (size_t)n, +1, ta_cmpoff, &o, buf); return; }
              memset(cnts, 0, sizeof(cnts));/* clear the pattern counters */
              for (mask = 0, t = tracts+n; --t >= tracts; ) {
                mask |= x = (*t)->items[o]; /* traverse the transactions, */
              cnts[x & 0x3f]++;           /* combine all bit patterns and */
              }                             /* count patterns with 6 bits */
              src = tracts; dst = buf;      /* get trans. source and destination */
              if (cnts[mask & 0x3f] < n) {  /* if more than one bit pattern */
              for (i = 0; ++i < 64; )     /* traverse the patterns and compute */
              cnts[i] += cnts[i-1];     /* offsets for storing transactions */
              for (t = src+n; --t >= src;)/* sort transactions with bin sort */
              dst[--cnts[(*t)->items[o] & 0x3f]] = *t;
              t = src; src = dst; dst = t;/* exchange source and destination, */
              }                             /* because trans. have been moved */
              for (k = 6; k < 31; k += 5) { /* traverse the remaining sections */
              x = (mask >> k) & 0x1f;     /* if all transactions are zero */
              if (!x) continue;           /* in a section, skip the section */
              memset(cnts, 0, 32*sizeof(TID)); /* clear pattern counters */
              for (t = src+n; --t >= src;)/* count the pattern occurrences */
              cnts[((*t)->items[o] >> k) & 0x1f]++;
              if (cnts[x] >= n) continue; /* check for only one pattern */
              for (i = 0; ++i < 32; )     /* traverse the patterns and compute */
              cnts[i] += cnts[i-1];     /* offsets for storing transactions */
              for (t = src+n; --t >= src;)/* sort transactions with bin sort */
              dst[--cnts[((*t)->items[o] >> k) & 0x1f]] = *t;
              t = src; src = dst; dst = t;/* exchange source and destination, */
              }                             /* because trans. have been moved */
              if (src != tracts)            /* ensure that result is in tracts */
              memcpy(tracts, src, (size_t)n *sizeof(TRACT*));
              }  /* pksort() */
              
              /*--------------------------------------------------------------------*/
              
              static void sort7 (TRACT **tracts, TID n, ITEM o,
                                 TRACT **buf, TID *cnts, ITEM k, ITEM mask)
              {                               /* --- sort trans. with bucket sort */
              TID   m;                      /* loop variable for transactions */
              ITEM  i, x, y;                /* loop variable, item buffers */
              TRACT **t;                    /* to traverse the transactions */
              
              assert(tracts && buf && cnts);/* check the function arguments */
              if (n <= 16) {                /* if only few transactions, */
              ptr_mrgsort(tracts, (size_t)n, +1,
                          (mask > ITEM_MIN) ? ta_cmpsfx : ta_cmpsep, &o, buf);
                return;                     /* sort the transactions plainly, */
              }                             /* then abort the function */
              memset(cnts-1, 0, (size_t)(k+1) *sizeof(TID));
              x = 0;                        /* clear the transaction counters */
              for (t = tracts+n; --t >= tracts; ) {
                x = (*t)->items[o];         /* traverse the transactions */
              if (x < 0) x = (x <= TA_END) ? -1 : 0;
              cnts[x]++;                  /* count the transactions per item */
              }                             /* (0 for packed items, -1 for none) */
              if (cnts[x] >= n) {           /* check for only one or no item */
              if (x < 0) return;          /* if all transactions end, abort */
              x = (*tracts)->items[o];    /* check for packed items */
              if ((x < 0) && (mask <= ITEM_MIN)) pksort(tracts, buf, n, o);
              sort7(tracts, n, o+1, buf, cnts, k, mask);
              if ((x < 0) && (mask >  ITEM_MIN)) pksort(tracts, buf, n, o);
              return;                     /* sort the whole array recursively */
              }                             /* and then abort the function */
              memcpy(buf, tracts, (size_t)n *sizeof(TRACT*));
              for (i = 0; i < k; i++)       /* traverse the items and compute */
              cnts[i] += cnts[i-1];       /* offsets for storing transactions */
              for (t = buf+n; --t >= buf; ) {
                x = (*t)->items[o];         /* traverse the transactions again */
              if (x < 0) x = (x <= TA_END) ? -1 : 0;
              tracts[--cnts[x]] = *t;     /* sort w.r.t. the item at offset o */
              }                             /* (0 for packed items, -1 for none) */
              tracts += m = cnts[0];        /* remove trans. that are too short */
              if ((n -= m) <= 0) return;    /* and if no others are left, abort */
              if ((*tracts)->items[o] < 0){ /* if there are packed items, sort */
              pksort(tracts, buf, m = cnts[1] -m, o);
                if (mask <= ITEM_MIN) {     /* if to treat packed items equally */
              sort7(tracts, m, o+1, buf, cnts, k, mask);
                  tracts += m;              /* sort suffixes of packed trans. */
              if ((n -= m) <= 0) return;/* and if no other transactions */
                }                           /* are left, abort the function */
              }                             /* traverse the formed sections */
              if ((x = (*tracts)->items[o]) < 0) x &= mask;
              for (t = tracts; --n > 0; ) { /* traverse the transactions */
              if ((y = (*++t)->items[o]) < 0) y &= mask;
              if (y == x) continue;       /* if same start item, continue */
              x = y;                      /* note the new start item */
              if ((m = (TID)(t-tracts)) > 1)
                sort7(tracts, m, o+1, buf, cnts, k, mask);
              tracts = t;                 /* sort the section recursively */
              }                             /* and skip the transactions in it */
              if ((m = (TID)((t+1)-tracts)) > 1)
                sort7(tracts, m, o+1, buf, cnts, k, mask);
              }  /* sort7() */                 /* finally sort the last section */
              
              /*--------------------------------------------------------------------*/
              
              void tbg_sort (TABAG *bag, int dir, int mode)
              {                               /* --- sort a transaction bag */
              ITEM  k;                      /* number of items */
              TID   n;                      /* number of transactions */
              ITEM  mask;                   /* mask for packed item treatment */
              TRACT **buf;                  /* trans. buffer for bucket sort */
              TID   *cnts;                  /* counter array for bin sort */
              CMPFN *cmp;                   /* comparison function */
              
              assert(bag);                  /* check the function arguments */
              if (bag->cnt < 2) return;     /* check for at least two trans. */
              n = bag->cnt;                 /* get the number of transactions */
              k = ib_cnt(bag->base);        /* and the number of items */
              if (k < 2) k = 2;             /* need at least 2 counters */
              if (bag->mode & IB_WEIGHTS) { /* if the items carry weights, */
              if (mode & TA_HEAP)         /* sort with weighted item functions */
              ptr_heapsort(bag->tracts, (size_t)n, dir, wta_cmp, NULL);
              else ptr_qsort   (bag->tracts, (size_t)n, dir, wta_cmp, NULL); }
              else if ((buf = (TRACT**)malloc((size_t) n   *sizeof(TRACT*)
                                                +(size_t)(k+1)*sizeof(TID)))) {
                                                if ((size_t)k < (size_t)n){ /* if bin sort is possible/favorable, */
              cnts = (TID*)(buf+n)+1;   /* use bin sort to sort transactions */
              mask = (mode & TA_EQPACK) ? ITEM_MIN : -1;
              sort7((TRACT**)bag->tracts, n, 0, buf, cnts, k, mask);
              if (dir < 0)              /* if necessary, reverse the order */
              ptr_reverse(bag->tracts, (size_t)n); }
                                                else {                      /* if more items than transactions */
              cmp = (mode & TA_EQPACK) ? ta_cmpep : ta_cmp;
                                                  ptr_mrgsort(bag->tracts, (size_t)n, dir, cmp, NULL, buf);
                                                }                           /* sort transactions with merge sort */
              free(buf); }                /* delete the allocated buffer */
              else {                        /* if to use failsafe functions */
              cmp = (mode & TA_EQPACK) ? ta_cmpep : ta_cmp;
                if (mode & TA_HEAP)         /* sort the transactions */
              ptr_heapsort(bag->tracts, (size_t)n, dir, cmp, NULL);
                else ptr_qsort   (bag->tracts, (size_t)n, dir, cmp, NULL);
              }                             /* use heapsort or quicksort */
              }  /* tbg_sort() */
              
              /*--------------------------------------------------------------------*/
              
              TID tbg_reduce (TABAG *bag, int keep0)
              {                               /* --- reduce a transaction bag */
              /* This function presupposes that the transaction bag has been */
              /* sorted with one of the above sorting functions beforehand.  */
              TID   i;                      /* loop variable */
              int   c;                      /* comparison result */
              TRACT **s, **d;               /* to traverse the transactions */
              
              assert(bag);                  /* check the function argument */
              if (bag->cnt <= 1) return 1;  /* deal only with two or more trans. */
              if (bag->icnts) {             /* delete the item-specific counters */
              free(bag->icnts); bag->icnts = NULL; bag->ifrqs = NULL; }
              bag->extent = 0;              /* reinit. number of item occurrences */
              s = d = (TRACT**)bag->tracts; /* traverse the sorted transactions */
              for (i = bag->cnt; --i > 0; ) {
                if (((*++s)->size != (*d)->size))
                  c = ((*s)->size < (*d)->size) ? -1 : +1;
                else                        /* compare items for same size */
              c = (bag->mode & IB_WEIGHTS) ? wta_cmp(*s, *d, NULL)
                  :  ta_cmp(*s, *d, NULL);
                if (c == 0) {               /* if the transactions are equal */
              (*d)->wgt += (*s)->wgt;   /* combine the transactions */
              free(*s); }               /* by summing their weights */
              else {                      /* if transactions are not equal */
              if (keep0 || ((*d)->wgt != 0))
                bag->extent += (size_t)(*d++)->size;
              else free(*d);            /* check weight of old transaction */
              *d = *s;                  /* copy the new transaction */
              }                           /* to close a possible gap */
              }                             /* (collect unique transactions) */
              if (keep0 || ((*d)->wgt != 0))
                bag->extent += (size_t)(*d++)->size;
              else free(*d);                /* check weight of last transaction */
              return bag->cnt = (TID)(d -(TRACT**)bag->tracts);
              }  /* tbg_reduce() */           /* return new number of transactions */
              
              /*--------------------------------------------------------------------*/
              
              void tbg_pack (TABAG *bag, int n)
              {                               /* --- pack all transactions */
              TID i;                        /* loop variable */
              
              assert(bag                    /* check the function arguments */
              &&   !(bag->mode & IB_WEIGHTS));
              if (n <= 0) return;           /* if no items to pack, abort */
              for (i = 0; i < bag->cnt; i++)/* pack items in all transactions */
              ta_pack((TRACT*)bag->tracts[i], n);
              bag->mode |= n & TA_PACKED;   /* set flag for packed transactions */
              }  /* tbg_pack() */
              
              /*--------------------------------------------------------------------*/
              
              void tbg_unpack (TABAG *bag, int dir)
              {                               /* --- unpack all transactions */
              TID i;                        /* loop variable */
              
              assert(bag                    /* check the function arguments */
              &&   !(bag->mode & IB_WEIGHTS));
              for (i = 0; i < bag->cnt; i++)/* pack items in all transactions */
              ta_unpack((TRACT*)bag->tracts[i], dir);
              bag->mode &= ~TA_PACKED;      /* clear flag for packed transactions */
              }  /* tbg_unpack() */
              /*----------------------------------------------------------------------
              Transaction Tree Functions
              ----------------------------------------------------------------------*/
              TANODE* tan_child (const TANODE *node, ITEM index)
              {                               /* --- get child of a tree node */
              TANODE **chn;                 /* array of child nodes */
              
              assert(node);                 /* check the function argument */
              chn = (TANODE**)(node->items +node->size);
              ALIGN(chn);                   /* get the child pointer array */
              return chn[index];            /* return the requested child */
              }  /* tan_child() */
              
              /*--------------------------------------------------------------------*/
              
              void delete_tract(TANODE *root)
              {                               /* --- delete a transaction (sub)tree */
              ITEM   i;                     /* loop variable */
              TANODE **chn;                 /* array of child nodes */
              
              assert(root);                 /* check the function argument */
              chn = (TANODE**)(root->items +root->size);
              ALIGN(chn);                   /* get the child pointer array */
              for (i = 0; i < root->size; i++)
                delete_tract(chn[i]);             /* recursively delete the subtrees */
              free(root);                   /* and the tree node itself */
              }  /* delete() */
              
              /*--------------------------------------------------------------------*/
              
              TANODE* create (TRACT **tracts, TID cnt, ITEM index)
              {                               /* --- recursive part of tat_create() */
              TID    i;                     /* loop variable */
              ITEM   item, k, n;            /* item identifier and counter */
              SUPP   w;                     /* item weight */
              size_t z;                     /* size of the node with item array */
              TANODE *node;                 /* node of created transaction tree */
              TANODE **chn;                 /* array of child nodes */
              
              assert(tracts                 /* check the function arguments */
              &&    (cnt > 0) && (index >= 0));
              if (cnt <= 1) {               /* if only one transaction left */
              n    = (*tracts)->size -index;
                node = (TANODE*)malloc(sizeof(TANODE) +(size_t)(n-1) *sizeof(ITEM));
                if (!node) return NULL;     /* create a transaction tree node */
              node->wgt  = (*tracts)->wgt;/* and initialize the fields */
              node->size = -(node->max = n);
              if (n > 0)                  /* copy the transaction suffix */
              memcpy(node->items, (*tracts)->items +index,
                     (size_t)n *sizeof(ITEM));
              return node;                /* copy the remaining items and */
              }                             /* return the created leaf node */
              
              for (w = 0; (cnt > 0) && ((*tracts)->size <= index); cnt--)
                w += (*tracts++)->wgt;      /* skip trans. that are too short */
              for (n = 0, item = TA_END, i = cnt; --i >= 0; ) {
                w += tracts[i]->wgt;        /* traverse the transactions */
              k  = tracts[i]->items[index];
              if (k != item) { item = k; n++; }
              }                             /* count the different items */
              z = sizeof(TANODE) +(size_t)(n-1) *sizeof(ITEM);
              node = (TANODE*)malloc(z +PAD(z) +(size_t)n *sizeof(TANODE*));
              if (!node) return NULL;       /* create a transaction tree node */
              node->wgt  = w;               /* and initialize its fields */
              node->max  = 0;
              node->size = n;               /* if all transactions are captured, */
              if (n <= 0) return node;      /* return the created tree */
              chn = (TANODE**)(node->items +n);
              ALIGN(chn);                   /* get the child pointer array */
              for (--cnt; --n >= 0; cnt = i) { /* traverse the different items */
              node->items[n] = item = tracts[cnt]->items[index];
                for (i = cnt; --i >= 0; )   /* find trans. with the current item */
              if (tracts[i]->items[index] != item) break;
              chn[n] = create(tracts+i+1, cnt-i, index+1);
              if (!chn[n]) break;         /* recursively create a subtree */
              if ((k = chn[n]->max +1) > node->max) node->max = k;
              }                             /* adapt the maximal remaining size */
              if (n < 0) return node;       /* if successful, return created tree */
              
              while (++n < node->size) delete_tract(chn[n]);
              free(node);                   /* on error delete created subtree */
              return NULL;                  /* return 'failure' */
              }  /* create() */
              /*----------------------------------------------------------------------
              Functions
              ----------------------------------------------------------------------*/
              double logGamma   (double n);
              double Gamma      (double n);
              double GammaP     (double n, double x);
              double GammaQ     (double n, double x);
              
#define LN_BASE      2.71828182845904523536028747135  /* e */
#define SQRT_PI      1.77245385090551602729816748334  /* \sqrt(\pi) */
#define LN_PI        1.14472988584940017414342735135  /* \ln(\pi) */
#define LN_SQRT_2PI  0.918938533204672741780329736406
              /* \ln(\sqrt(2\pi)) */
#define EPSILON      2.2204460492503131e-16
#define EPS_QTL      1.4901161193847656e-08
#define MAXFACT      170
#define MAXITER      1024
#define TINY         (EPSILON *EPSILON *EPSILON)
              
              /*----------------------------------------------------------------------
              Table of Factorials/Gamma Values
----------------------------------------------------------------------*/
              static double facts[MAXFACT+1] = { 0 };
              static double logfs[MAXFACT+1];
              static double halfs[MAXFACT+1];
              static double loghs[MAXFACT+1];
              
              /*----------------------------------------------------------------------
              Functions
              ----------------------------------------------------------------------*/
              
              static void init (void)
              {                               /* --- init. factorial tables */
              int    i;                     /* loop variable */
              double x = 1;                 /* factorial */
              
              facts[0] = facts[1] = 1;      /* store factorials for 0 and 1 */
              logfs[0] = logfs[1] = 0;      /* and their logarithms */
              for (i = 1; ++i <= MAXFACT; ) {
                facts[i] = x *= i;          /* initialize the factorial table */
              logfs[i] = log(x);          /* and the table of their logarithms */
              }
              halfs[0] = x = SQRT_PI;       /* store Gamma(0.5) */
              loghs[0] = 0.5*LN_PI;         /* and its logarithm */
              for (i = 0; ++i < MAXFACT; ) {
                halfs[i] = x *= i-0.5;      /* initialize the table for */
              loghs[i] = log(x);          /* the Gamma function of half numbers */
              }                             /* and the table of their logarithms */
              }  /* init() */
              
              /*--------------------------------------------------------------------*/
              double logGamma (double n)
              {                               /* --- compute ln(Gamma(n))         */
              double s;                     /*           = ln((n-1)!), n \in IN */
              
              assert(n > 0);                /* check the function argument */
              if (facts[0] <= 0) init();    /* initialize the tables */
              if (n < MAXFACT +1 +4 *EPSILON) {
                if (fabs(  n -floor(  n)) < 4 *EPSILON)
                  return logfs[(int)floor(n)-1];
                if (fabs(2*n -floor(2*n)) < 4 *EPSILON)
                  return loghs[(int)floor(n)];
              }                             /* try to get the value from a table */
              s =    0.99999999999980993227684700473478  /* otherwise compute it */
              +  676.520368121885098567009190444019 /(n+1)
                - 1259.13921672240287047156078755283  /(n+2)
                +  771.3234287776530788486528258894   /(n+3)
                -  176.61502916214059906584551354     /(n+4)
                +   12.507343278686904814458936853    /(n+5)
                -    0.13857109526572011689554707     /(n+6)
                +    9.984369578019570859563e-6       /(n+7)
                +    1.50563273514931155834e-7        /(n+8);
                return (n+0.5) *log((n+7.5)/LN_BASE) +(LN_SQRT_2PI +log(s/n) -7.0);
              }  /* logGamma() */
              
              /*----------------------------------------------------------------------
              Use Lanczos' approximation
              \Gamma(n+1) = (n+\gamma+0.5)^(n+0.5)
              * e^{-(n+\gamma+0.5)}
              * \sqrt{2\pi}
              * (c_0 +c_1/(n+1) +c_2/(n+2) +...+c_n/(n+k) +\epsilon)
              and exploit the recursion \Gamma(n+1) = n *\Gamma(n) once,
              i.e., compute \Gamma(n) as \Gamma(n+1) /n.
              
              For the choices \gamma = 5, k = 6, and c_0 to c_6 as defined
              in the first version, it is |\epsilon| < 2e-10 for all n > 0.
              
              Source: W.H. Press, S.A. Teukolsky, W.T. Vetterling, and B.P. Flannery
              Numerical Recipes in C - The Art of Scientific Computing
              Cambridge University Press, Cambridge, United Kingdom 1992
              pp. 213-214
              
              For the choices gamma = 7, k = 8, and c_0 to c_8 as defined
              in the second version, the value is slightly more accurate.
              ----------------------------------------------------------------------*/
              
              double Gamma (double n)
              {                               /* --- compute Gamma(n) = (n-1)! */
              assert(n > 0);                /* check the function argument */
              if (facts[0] <= 0) init();    /* initialize the tables */
              if (n < MAXFACT +1 +4 *EPSILON) {
                if (fabs(  n -floor(  n)) < 4 *EPSILON)
                  return facts[(int)floor(n)-1];
                if (fabs(2*n -floor(2*n)) < 4 *EPSILON)
                  return halfs[(int)floor(n)];
              }                             /* try to get the value from a table */
              return exp(logGamma(n));      /* compute through natural logarithm */
              }  /* Gamma() */
              
              /*--------------------------------------------------------------------*/
              
              static double series (double n, double x)
              {                               /* --- series approximation */
              int    i;                     /* loop variable */
              double t, sum;                /* buffers */
              
              sum = t = 1/n;                /* compute initial values */
              for (i = MAXITER; --i >= 0; ) {
                sum += t *= x/++n;          /* add one term of the series */
              if (fabs(t) < fabs(sum) *EPSILON) break;
              }                             /* if term is small enough, abort */
              return sum;                   /* return the computed factor */
              }  /* series() */
              
              /*----------------------------------------------------------------------
              series approximation:
              P(a,x) =    \gamma(a,x)/\Gamma(a)
              \gamma(a,x) = e^-x x^a \sum_{n=0}^\infty (\Gamma(a)/\Gamma(a+1+n)) x^n
              
              Source: W.H. Press, S.A. Teukolsky, W.T. Vetterling, and B.P. Flannery
              Numerical Recipes in C - The Art of Scientific Computing
              Cambridge University Press, Cambridge, United Kingdom 1992
              formula: pp. 216-219
              
              The factor exp(n *log(x) -x) is added in the functions below.
              ----------------------------------------------------------------------*/
              
              static double cfrac (double n, double x)
              {                               /* --- continued fraction approx. */
              int    i;                     /* loop variable */
              double a, b, c, d, e, f;      /* buffers */
              
              b = x+1-n; c = 1/TINY; f = d = 1/b;
              for (i = 1; i < MAXITER; i++) {
                a = i*(n-i);                /* use Lentz's algorithm to compute */
              d = a *d +(b += 2);         /* consecutive approximations */
              if (fabs(d) < TINY) d = TINY;
              c = b +a/c;
              if (fabs(c) < TINY) c = TINY;
              d = 1/d; f *= e = d *c;
              if (fabs(e-1) < EPSILON) break;
              }                             /* if factor is small enough, abort */
              return f;                     /* return the computed factor */
              }  /* cfrac() */
              
              /*--------------------------------------------------------------------*/
              
              double GammaP (double n, double x)
              {                               /* --- regularized Gamma function P */
              assert((n > 0) && (x >= 0));  /* check the function arguments */
              if (x <=  0) return 0;        /* treat x = 0 as a special case */
              if (x < n+1) return series(n, x) *exp(n *log(x) -x -logGamma(n));
              return 1 -cfrac(n, x) *exp(n *log(x) -x -logGamma(n));
              }  /* GammaP() */
              
              /*--------------------------------------------------------------------*/
              
              double GammaQ (double n, double x)
              {                               /* --- regularized Gamma function Q */
              assert((n > 0) && (x >= 0));  /* check the function arguments */
              if (x <=  0) return 1;        /* treat x = 0 as a special case */
              if (x < n+1) return 1 -series(n, x) *exp(n *log(x) -x -logGamma(n));
              return cfrac(n, x) *exp(n *log(x) -x -logGamma(n));
              }  /* GammaQ() */
              /* --- rule evaluation function identifiers --- */
#define RE_NONE        0        /* no measure / constant zero */
#define RE_SUPP        1        /* rule support (body and head) */
#define RE_CONFIDENCE  2        /* rule confidence */
#define RE_CONF        2        /* rule confidence */
#define RE_CONFDIFF    3        /* confidence diff. to prior */
#define RE_LIFT        4        /* lift value (conf./prior) */
#define RE_LIFTDIFF    5        /* difference of lift value    to 1 */
#define RE_LIFTQUOT    6        /* difference of lift quotient to 1 */
#define RE_CONVICTION  7        /* conviction */
#define RE_CVCT        7        /* conviction */
#define RE_CVCTDIFF    8        /* difference of conviction  to 1 */
#define RE_CVCTQUOT    9        /* difference of conv. quot. to 1 */
#define RE_CPROB      10        /* conditional probability ratio */
#define RE_IMPORT     11        /* importance */
#define RE_IMPORTANCE 11        /* importance */
#define RE_CERTAINTY  12        /* certainty factor */
#define RE_CERT       12        /* certainty factor */
#define RE_CHI2       13        /* normalized chi^2 measure */
#define RE_CHI2PVAL   14        /* p-value from chi^2 measure */
#define RE_YATES      15        /* normalized chi^2 measure (Yates) */
#define RE_YATESPVAL  16        /* p-value from chi^2 measure (Yates) */
#define RE_INFO       17        /* information diff. to prior */
#define RE_INFOPVAL   18        /* p-value from info diff. */
#define RE_FETPROB    19        /* Fisher's exact test (prob.) */
#define RE_FETCHI2    20        /* Fisher's exact test (chi^2) */
#define RE_FETINFO    21        /* Fisher's exact test (info.) */
#define RE_FETSUPP    22        /* Fisher's exact test (supp.) */
#define RE_FNCNT      23        /* number of evaluation functions */
              
#define LN_2        0.69314718055994530942  /* ln(2) */
              
              /*----------------------------------------------------------------------
              Type Definitions
----------------------------------------------------------------------*/
              typedef double RULEVALFN (SUPP supp, SUPP body, SUPP head, SUPP base);
              
              RULEVALFN* re_function (int id);
              int        re_dir      (int id);
              
              
              /*----------------------------------------------------------------------
              Functions
              ----------------------------------------------------------------------*/
              
              double chi2cdfQDEL (double x, double df)
              {                               /* --- cumulative distribution fn. */
              assert(df > 0);               /* check the function arguments */
              return GammaQ(0.5*df, 0.5*x); /* compute regularized Gamma function */
              }  /* chi2cdfQ() */
              
              /*----------------------------------------------------------------------
              Type Definitions
              ----------------------------------------------------------------------*/
              typedef struct {                /* --- rule evaluation info. --- */
              RULEVALFN *fn;                /* evaluation function */
              int       dir;                /* evaluation direction */
              } REINFO;                       /* (rule evaluation information) */
              
              /*----------------------------------------------------------------------
              Rule Evaluation Measures
              ----------------------------------------------------------------------*/
              
              double re_none (SUPP supp, SUPP body, SUPP head, SUPP base)
              { return 0; }                   /* --- no measure / constant zero */
              
              /*--------------------------------------------------------------------*/
              
              double re_supp (SUPP supp, SUPP body, SUPP head, SUPP base)
              { return (double)supp; }        /* --- rule support (body and head) */
              
              /*--------------------------------------------------------------------*/
              
              double re_conf (SUPP supp, SUPP body, SUPP head, SUPP base)
              {                               /* --- rule confidence */
              return (body > 0) ? (double)supp/(double)body : 0;
              }  /* re_conf() */
              
              /*--------------------------------------------------------------------*/
              
              double re_confdiff (SUPP supp, SUPP body, SUPP head, SUPP base)
              {                               /* --- absolute confidence difference */
              if ((body <= 0) || (base <= 0)) return 0;
              return fabs((double)supp/(double)body -(double)head/(double)base);
              }  /* re_confdiff() */
              
              /*--------------------------------------------------------------------*/
              
              double re_lift (SUPP supp, SUPP body, SUPP head, SUPP base)
              {                               /* --- lift value */
              if ((body <= 0) || (head <= 0)) return 0;
              return ((double)supp*(double)base) /((double)body*(double)head);
              /* =   ((double)supp/(double)body) /((double)head/(double)base) */
              }  /* re_lift() */
              
              /*--------------------------------------------------------------------*/
              
              double re_liftdiff (SUPP supp, SUPP body, SUPP head, SUPP base)
              {                               /* --- abs. difference of lift to 1 */
              if ((body <= 0) || (head <= 0)) return 0;
              return fabs(((double)supp*(double)base)
                            /((double)body*(double)head) -1);
              }  /* re_liftdiff() */
              
              /*--------------------------------------------------------------------*/
              
              double re_liftquot (SUPP supp, SUPP body, SUPP head, SUPP base)
              {                               /* --- diff. of lift quotient to 1 */
              double t;                     /* temporary buffer */
              
              if ((body <= 0) || (head <= 0)) return 0;
              t = ((double)supp*(double)base) /((double)body*(double)head);
              return 1 -((t > 1) ? 1/t : t);
              }  /* re_liftquot() */
              
              /*--------------------------------------------------------------------*/
              
              double re_cvct (SUPP supp, SUPP body, SUPP head, SUPP base)
              {                               /* --- conviction */
              if ((base <= 0) || (body <= supp)) return 0;
              return ((double)body*(double)(base-head))
                / ((double)(body-supp)*(double)base);
              /*   = ((double)body/(double)(body-supp))
              * ((double)(base-head)/(double)base); */
              }  /* re_cvct() */
              
              /*--------------------------------------------------------------------*/
              
              double re_cvctdiff (SUPP supp, SUPP body, SUPP head, SUPP base)
              {                               /* --- abs. diff. of conviction to 1 */
              if (base <= 0)    return 0;
              if (body <= supp) return INFINITY;
              return fabs(((double)body*(double)(base-head))
                            /((double)(body-supp)*(double)base)-1);
              }  /* re_cvctdiff() */
              
              /*--------------------------------------------------------------------*/
              
              double re_cvctquot (SUPP supp, SUPP body, SUPP head, SUPP base)
              {                               /* --- diff. of conviction quot. to 1 */
              double t;                     /* temporary buffer */
              
              if (base <= 0)    return 0;
              if (body <= supp) return INFINITY;
              t = ((double)body*(double)(base-head))
                /((double)(body-supp)*(double)base);
              return 1 -((t > 1) ? 1/t : t);
              }  /* re_cvctquot() */
              
              /*--------------------------------------------------------------------*/
              
              double re_cprob (SUPP supp, SUPP body, SUPP head, SUPP base)
              {                               /* --- conditional probability ratio */
              if ((supp <= 0) || (body <= 0) || (base <= body)) return 0;
              if (head <= supp) return INFINITY;
              return ((double)supp*(double)(base-body))
                / ((double)body*(double)(head-supp));
              }  /* re_cprob() */
              
              /*--------------------------------------------------------------------*/
              
              double re_import (SUPP supp, SUPP body, SUPP head, SUPP base)
              {                               /* --- importance */
              double r;                     /* conditional probability ratio */
              
              if ((supp <= 0) || (body <= 0) || (base <= body)) return 0;
              if (head <= supp) return INFINITY;
              r = ((double)supp*(double)(base-body))
                / ((double)body*(double)(head-supp));
              return (r > 0) ? log(r)/LN_2 : 0;
              }  /* re_import() */
              
              /*--------------------------------------------------------------------*/
              
              double re_cert (SUPP supp, SUPP body, SUPP head, SUPP base)
              {                               /* --- certainty factor */
              double n, p;                  /* temporary buffers */
              
              if ((body <= 0) || (base <= 0)) return 0;
              p = (double)head/(double)base; n = (double)supp/(double)body -p;
              return n / ((n >= 0) ? 1-p : p);
              }  /* re_cert() */
              
              /*--------------------------------------------------------------------*/
              
              double re_chi2 (SUPP supp, SUPP body, SUPP head, SUPP base)
              {                               /* --- normalized chi^2 measure */
              double t;                     /* temporary buffer */
              
              if ((head <= 0) || (head >= base)
                    ||  (body <= 0) || (body >= base))
                return 0;                   /* check for non-vanishing marginals */
              t = (double)head *(double)body -(double)supp *(double)base;
              return (t*t) /(((double)head)*(double)(base-head)
                               *((double)body)*(double)(base-body));
              }  /* re_chi2() */              /* compute and return chi^2 measure */
              
              /*--------------------------------------------------------------------*/
              
              double re_chi2pval (SUPP supp, SUPP body, SUPP head, SUPP base)
              {                               /* --- p-value from chi^2 measure */
              return chi2cdfQDEL((double)base *re_chi2(supp, body, head, base), 1);
              }  /* re_chi2pval() */
              
              /*--------------------------------------------------------------------*/
              
              double re_yates (SUPP supp, SUPP body, SUPP head, SUPP base)
              {                               /* --- Yates corrected chi^2 measure */
              double t;                     /* temporary buffer */
              
              if ((head <= 0) || (head >= base)
                    ||  (body <= 0) || (body >= base))
                return 0;                   /* check for non-vanishing marginals */
              t = fabs((double)head *(double)body -(double)supp *(double)base)
                - 0.5*(double)base;
              return (t*t) /(((double)head)*(double)(base-head)
                               *((double)body)*(double)(base-body));
              }  /* re_yates() */             /* compute and return chi^2 measure */
              
              /*--------------------------------------------------------------------*/
              
              double re_yatespval (SUPP supp, SUPP body, SUPP head, SUPP base)
              {                               /* --- p-value from chi^2 measure */
              return chi2cdfQDEL((double)base *re_yates(supp, body, head, base), 1);
              }  /* re_yatespval() */
              
              /*--------------------------------------------------------------------*/
              
              double re_info (SUPP supp, SUPP body, SUPP head, SUPP base)
              {                               /* --- information diff. to prior */
              double sum, t;                /* result, temporary buffer */
              
              if ((head <= 0) || (head >= base)
                    ||  (body <= 0) || (body >= base))
                return 0;                   /* check for strict positivity */
              t = (double)supp; sum = 0;
              if (t > 0)                    /* support of     head and     body */
              sum += t *log(t /((double)      head  *(double)      body ));
              t = (double)(body -supp);
              if (t > 0)                    /* support of not head and     body */
              sum += t *log(t /((double)(base-head) *(double)      body ));
              t = (double)(head -supp);
              if (t > 0)                    /* support of     head and not body */
              sum += t *log(t /((double)      head  *(double)(base-body)));
              t = (double)(base -head -body +supp);
              if (t > 0)                    /* support of not head and not body */
              sum += t *log(t /((double)(base-head) *(double)(base-body)));
              return (log((double)base) +sum/(double)base) /LN_2;
              }  /* re_info() */              /* return information gain in bits */
              
              /*--------------------------------------------------------------------*/
              
              double re_infopval (SUPP supp, SUPP body, SUPP head, SUPP base)
              {                               /* --- p-value from G statistic */
              return chi2cdfQDEL(2*LN_2 *(double)base
                                   *re_info(supp, body, head, base), 1);
              }  /* re_infopval() */
              
              /*--------------------------------------------------------------------*/
              
              double re_fetprob (SUPP supp, SUPP body, SUPP head, SUPP base)
              {                               /* --- Fisher's exact test (prob.) */
              SUPP   rest, n;               /* counter for rest cases, buffer */
              double com;                   /* common probability term */
              double cut, p;                /* (cutoff value for) probability */
              double sum;                   /* probability sum of conting. tables */
              
              if ((head <= 0) || (head >= base)
                    ||  (body <= 0) || (body >= base))
                return 1;                   /* check for non-vanishing marginals */
              rest = base -head -body;      /* compute number of rest cases */
              if (rest < 0) {               /* if rest cases are less than supp, */
              supp -= rest = -rest;       /* exchange rows and exchange columns */
              body  = base -body; head = base -head;
              }                             /* complement/exchange the marginals */
              if (head < body) {            /* ensure that body <= head */
              n = head; head = body; body = n; }
              com = logGamma((double)(     head+1))
                + logGamma((double)(     body+1))
                + logGamma((double)(base-head+1))
                + logGamma((double)(base-body+1))
                - logGamma((double)(base+1));/* compute common probability term */
              cut = com                     /* and log of the cutoff probability */
              - logGamma((double)(body-supp+1))
                - logGamma((double)(head-supp+1))
                - logGamma((double)(     supp+1))
                - logGamma((double)(rest+supp+1));
                cut *= 1.0-DBL_EPSILON;       /* adapt for roundoff errors */
              /* cut must be multiplied with a value < 1 in order to increase it, */
              /* because it is the logarithm of a probability and hence negative. */
              for (sum = 0, supp = 0; supp <= body; supp++) {
                p = com                     /* traverse the contingency tables */
              - logGamma((double)(body-supp+1))
                - logGamma((double)(head-supp+1))
                - logGamma((double)(     supp+1))
                - logGamma((double)(rest+supp+1));
                if (p <= cut) sum += exp(p);/* sum probabilities greater */
              }                             /* than the cutoff probability */
              return sum;                   /* return computed probability */
              }  /* re_fetprob() */
              
              /*--------------------------------------------------------------------*/
              
              double re_fetchi2 (SUPP supp, SUPP body, SUPP head, SUPP base)
              {                               /* --- Fisher's exact test (chi^2) */
              SUPP   rest, n;               /* counter for rest cases, buffer */
              double com;                   /* common probability term */
              double exs;                   /* expected support value */
              double sum;                   /* probability sum of conting. tables */
              
              if ((head <= 0) || (head >= base)
                    ||  (body <= 0) || (body >= base))
                return 1;                   /* check for non-vanishing marginals */
              rest = base -head -body;      /* compute number of rest cases */
              if (rest < 0) {               /* if rest cases are less than supp, */
              supp -= rest = -rest;       /* exchange rows and exchange columns */
              body  = base -body; head = base -head;
              }                             /* complement/exchange the marginals */
              if (head < body) {            /* ensure that body <= head */
              n = head; head = body; body = n; }
              com = logGamma((double)(     head+1))
                + logGamma((double)(     body+1))
                + logGamma((double)(base-head+1))
                + logGamma((double)(base-body+1))
                - logGamma((double)(base+1));/* compute common probability term */
              exs = (double)head *(double)body /(double)base;
              if ((double)supp < exs)
              { n =              (SUPP)ceil (exs+(exs-(double)supp)); }
              else { n = supp; supp = (SUPP)floor(exs-((double)supp-exs)); }
              if (n > body) n = body+1;     /* compute the range of values and */
              if (supp < 0) supp = -1;      /* clamp it to the possible maximum */
              if (n-supp-4 < supp+body-n) { /* if fewer less extreme tables */
              for (sum = 1; ++supp < n;){ /* traverse the less extreme tables */
              sum -= exp(com -logGamma((double)(body-supp+1))
                           -logGamma((double)(head-supp+1))
                           -logGamma((double)(     supp+1))
                           -logGamma((double)(rest+supp+1)));
              } }                         /* sum the probability of the tables */
              else {                        /* if fewer more extreme tables */
              for (sum = 0; supp >= 0; supp--) {
                sum += exp(com -logGamma((double)(body-supp+1))
                             -logGamma((double)(head-supp+1))
                             -logGamma((double)(     supp+1))
                             -logGamma((double)(rest+supp+1)));
              }                           /* traverse the more extreme tables */
              for (supp = n; supp <= body; supp++) {
                sum += exp(com -logGamma((double)(body-supp+1))
                             -logGamma((double)(head-supp+1))
                             -logGamma((double)(     supp+1))
                             -logGamma((double)(rest+supp+1)));
              }                           /* sum the probability of the tables */
              }                             /* (upper and lower table ranges) */
              return sum;                   /* return computed probability */
              }  /* re_fetchi2() */
              
              /*--------------------------------------------------------------------*/
              
              double re_fetinfo (SUPP supp, SUPP body, SUPP head, SUPP base)
              {                               /* --- Fisher's exact test (info.) */
              SUPP   rest, n;               /* counter for rest cases, buffer */
              double com;                   /* common probability term */
              double cut;                   /* cutoff value for information gain */
              double sum;                   /* probability sum of conting. tables */
              
              if ((head <= 0) || (head >= base)
                    ||  (body <= 0) || (body >= base))
                return 1;                   /* check for non-vanishing marginals */
              rest = base -head -body;      /* compute number of rest cases */
              if (rest < 0) {               /* if rest cases are less than supp, */
              supp -= rest = -rest;       /* exchange rows and exchange columns */
              body  = base -body; head = base -head;
              }                             /* complement/exchange the marginals */
              if (head < body) {            /* ensure that body <= head */
              n = head; head = body; body = n; }
              com = logGamma((double)(     head+1))
                + logGamma((double)(     body+1))
                + logGamma((double)(base-head+1))
                + logGamma((double)(base-body+1))
                - logGamma((double)(base+1));/* compute common probability term */
              cut = re_info(supp, body, head, base) *(1.0-DBL_EPSILON);
              for (sum = 0, supp = 0; supp <= body; supp++) {
                if (re_info(supp, body, head, base) >= cut)
                  sum += exp(com -logGamma((double)(body-supp+1))
                               -logGamma((double)(head-supp+1))
                               -logGamma((double)     (supp+1))
                               -logGamma((double)(rest+supp+1)));
              }                             /* sum probs. of less extreme tables */
              return sum;                   /* return computed probability */
              }  /* re_fetinfo() */
              
              /*--------------------------------------------------------------------*/
              
              double re_fetsupp (SUPP supp, SUPP body, SUPP head, SUPP base)
              {                               /* --- Fisher's exact test (support) */
              SUPP   rest, n;               /* counter for rest cases, buffer */
              double com;                   /* common probability term */
              double sum;                   /* probability sum of conting. tables */
              
              if ((head <= 0) || (head >= base)
                    ||  (body <= 0) || (body >= base))
                return 1;                   /* check for non-vanishing marginals */
              rest = base -head -body;      /* compute number of rest cases */
              if (rest < 0) {               /* if rest cases are less than supp, */
              supp -= rest = -rest;       /* exchange rows and exchange columns */
              body  = base -body; head = base -head;
              }                             /* complement/exchange the marginals */
              if (head < body) {            /* ensure that body <= head */
              n = head; head = body; body = n; }
              com = logGamma((double)(     head+1))
                + logGamma((double)(     body+1))
                + logGamma((double)(base-head+1))
                + logGamma((double)(base-body+1))
                - logGamma((double)(base+1));/* compute common probability term */
              if (supp <= body -supp) {     /* if fewer lesser support values */
              for (sum = 1.0; --supp >= 0; )
                sum -= exp(com -logGamma((double)(body-supp+1))
                             -logGamma((double)(head-supp+1))
                             -logGamma((double)(     supp+1))
                             -logGamma((double)(rest+supp+1))); }
                             else {                        /* if fewer greater support values */
              for (sum = 0.0; supp <= body; supp++)
                sum += exp(com -logGamma((double)(body-supp+1))
                             -logGamma((double)(head-supp+1))
                             -logGamma((double)(     supp+1))
                             -logGamma((double)(rest+supp+1)));
                             }                             /* sum the table probabilities */
              return sum;                   /* return computed probability */
              }  /* re_fetsupp() */
              
              /*--------------------------------------------------------------------*/
              
              static const REINFO reinfo[] ={ /* --- rule evaluation functions */
              /* RE_NONE       0 */  { re_none,       0 },
              /* RE_SUPP       1 */  { re_supp,      +1 },
              /* RE_CONF       2 */  { re_conf,      +1 },
              /* RE_CONFDIFF   3 */  { re_confdiff,  +1 },
              /* RE_LIFT       4 */  { re_lift,      +1 },
              /* RE_LIFTDIFF   5 */  { re_liftdiff,  +1 },
              /* RE_LIFTQUOT   6 */  { re_liftquot,  +1 },
              /* RE_CVCT       7 */  { re_cvct,      +1 },
              /* RE_CVCTDIFF   8 */  { re_cvctdiff,  +1 },
              /* RE_CVCTQUOT   9 */  { re_cvctquot,  +1 },
              /* RE_CPROB     10 */  { re_cprob,     +1 },
              /* RE_IMPORT    11 */  { re_import,    +1 },
              /* RE_CERT      12 */  { re_cert,      +1 },
              /* RE_CHI2      13 */  { re_chi2,      +1 },
              /* RE_CHI2PVAL  14 */  { re_chi2pval,  -1 },
              /* RE_YATES     15 */  { re_yates,     +1 },
              /* RE_YATESPVAL 16 */  { re_yatespval, -1 },
              /* RE_INFO      17 */  { re_info,      +1 },
              /* RE_INFOPVAL  18 */  { re_infopval,  -1 },
              /* RE_FETPROB   19 */  { re_fetprob,   -1 },
              /* RE_FETCHI2   20 */  { re_fetchi2,   -1 },
              /* RE_FETINFO   21 */  { re_fetinfo,   -1 },
              /* RE_FETSUPP   21 */  { re_fetsupp,   -1 },
              };                              /* table of evaluation functions */
              
              /*--------------------------------------------------------------------*/
              
              RULEVALFN* re_function (int id)
              {                               /* --- get a rule evalution function */
              assert((id >= 0) && (id <= RE_FNCNT));
                return reinfo[id].fn;         /* retrieve function from table */
              }  /* re_function() */
              
              /*--------------------------------------------------------------------*/
              
              int re_dir (int id)
              {                               /* --- get a rule evalution direction */
              assert((id >= 0) && (id <= RE_FNCNT));
                return reinfo[id].dir;        /* retrieve direction from table */
              }  /* re_dir() */
              
#define RSUPP       SUPP        /* support type for reporting */
#define RSUPP_MIN   SUPP_MIN    /* minimum support value */
#define RSUPP_MAX   SUPP_MAX    /* maximum support value */
#define RSUPP_EPS   SUPP_EPS    /* minimum support step */
#define RSUPP_FMT   SUPP_FMT    /* printf format code for SUPP */
              
              /*----------------------------------------------------------------------
Preprocessor Definitions
----------------------------------------------------------------------*/
              /* --- estimation target indicators --- */
#define PSP_ITEMSET     0       /* item sets */
#define PSP_PERMUT      1       /* partial permutations (no reps.) */
#define PSP_SEQUENCE    2       /* sequences (with & without reps.) */
              
              /*----------------------------------------------------------------------
              Type Definitions
----------------------------------------------------------------------*/
              typedef struct {                /* --- pattern spectrum row -- */
              RSUPP  min, max;              /* minimum and maximum support */
              RSUPP  cur;                   /* current maximum support */
              size_t sum;                   /* sum of occurrences (for this size) */
              size_t *frqs;                 /* occurrence counters (by support) */
              } PSPROW;                       /* (pattern spectrum row) */
              
              typedef struct {                /* --- pattern spectrum --- */
              ITEM   minsize;               /* minimum pattern size (offset) */
              ITEM   maxsize;               /* maximum pattern size (limit) */
              RSUPP  minsupp;               /* minimum support (offset) */
              RSUPP  maxsupp;               /* maximum support (limit) */
              size_t sigcnt;                /* number of registered signatures */
              size_t total;                 /* total frequency of signatures */
              ITEM   max;                   /* number of pattern spectrum rows */
              ITEM   cur;                   /* current maximum used size/row */
              int    err;                   /* error status */
              PSPROW *rows;                 /* pattern spectrum rows (by size) */
              } PATSPEC;                      /* (pattern spectrum) */
              
              /*----------------------------------------------------------------------
              Functions
              ----------------------------------------------------------------------*/
              PATSPEC* psp_create  (ITEM  minsize, ITEM  maxsize,
                                    RSUPP minsupp, RSUPP maxsupp);
              void     psp_delete  (PATSPEC *psp);
              void     psp_clear   (PATSPEC *psp);
              ITEM     psp_min     (PATSPEC *psp);
              ITEM     psp_max     (PATSPEC *psp);
              RSUPP    psp_min4sz  (PATSPEC *psp, ITEM size);
              RSUPP    psp_max4sz  (PATSPEC *psp, ITEM size);
              int      psp_error   (PATSPEC *psp);
              size_t   psp_sigcnt  (PATSPEC *psp);
              size_t   psp_getfrq  (PATSPEC *psp, ITEM size, RSUPP supp);
              int      psp_incfrq  (PATSPEC *psp, ITEM size, RSUPP supp,
                                    size_t frq);
              
              /*----------------------------------------------------------------------
              Preprocessor Definitions
              ----------------------------------------------------------------------*/
#define psp_min(p)          ((p)->minsize)
#define psp_max(p)          ((p)->cur)
#define psp_min4sz(p,s)     ((p)->rows[s].min)
#define psp_max4sz(p,s)     ((p)->rows[s].cur)
#define psp_sum4sz(p,s)     ((p)->rows[s].sum)
#define psp_error(p)        ((p)->err)
#define psp_sigcnt(p)       ((p)->sigcnt)
              
              
              /*----------------------------------------------------------------------
              Preprocessor Definitions
----------------------------------------------------------------------*/
#define BLKSIZE      32         /* block size for enlarging arrays */
              
              
              /*----------------------------------------------------------------------
              Type Definitions
----------------------------------------------------------------------*/
              /*----------------------------------------------------------------------
              Global Variables
----------------------------------------------------------------------*/
              static PSPROW empty = { RSUPP_MAX, RSUPP_MIN, RSUPP_MIN, 0, NULL };
              /* an empty row entry for initializing new rows */
              
              
              /*----------------------------------------------------------------------
              Functions
              ----------------------------------------------------------------------*/
              
              PATSPEC* psp_create (ITEM  minsize, ITEM  maxsize,
                                   RSUPP minsupp, RSUPP maxsupp)
              {                               /* --- create pattern spectrum */
              PATSPEC *psp;                 /* created pattern spectrum */
              
              assert((minsize >= 0) && (maxsize >= minsize)   /* check the */
              &&     (minsupp >= 0) && (maxsupp >= minsupp)); /* arguments */
              psp = (PATSPEC*)malloc(sizeof(PATSPEC));
              if (!psp) return NULL;        /* create the base structure */
              psp->minsize = minsize;       /* and initialize the fields */
              psp->maxsize = ((maxsize < 0) || (maxsize >= ITEM_MAX))
                ? ITEM_MAX -1 : maxsize;
              psp->minsupp = minsupp;
              psp->maxsupp = ((maxsupp < 0) || (maxsupp >= RSUPP_MAX))
                ? RSUPP_MAX-1 : maxsupp;
              psp->total   = psp->sigcnt = 0;
              psp->cur     = psp->max    = minsize -1;
              psp->err     = 0;
              psp->rows    = NULL;
              return psp;                   /* return created pattern spectrum */
              }  /* psp_create() */
              
              /*--------------------------------------------------------------------*/
              
              void psp_delete (PATSPEC *psp)
              {                               /* --- delete pattern spectrum */
              assert(psp);                  /* check the function argument */
              if (psp->rows) {              /* if there are pattern spectrum rows */
              ITEM size;                  /* loop variable */
              for (size = psp->minsize; size < psp->max; size++)
                if (psp->rows[size].frqs) free(psp->rows[size].frqs);
                free(psp->rows);            /* delete the row array */
              }
              free(psp);                    /* delete the base structure */
              }  /* psp_delete() */
              
              /*--------------------------------------------------------------------*/
              
              void psp_clear (PATSPEC *psp)
              {                               /* --- clear pattern spectrum */
              ITEM size;                    /* loop variable */
              
              assert(psp);                  /* check the function argument */
              if (psp->rows) {              /* if there are pattern spectrum rows */
              for (size = psp->minsize; size < psp->max; size++) {
                if (psp->rows[size].frqs) free(psp->rows[size].frqs);
                psp->rows[size] = empty;  /* reinitialize the rows */
              }                           /* (also needed for RSUPP==double) */
              }
              psp->total = psp->sigcnt = 0; /* clear total and signature counter */
              psp->cur   = psp->max    = psp->minsize-1;
              psp->err   = 0;               /* clear the maximum size */
              }  /* psp_clear() */            /* and the error status */
              
              /*--------------------------------------------------------------------*/
              
              size_t psp_getfrq (PATSPEC *psp, ITEM size, RSUPP supp)
              {                               /* --- get a counter value */
              PSPROW *row;                  /* to access the table row */
              
              assert(psp);                  /* check the function arguments */
              if ((size < psp->minsize) || (size > psp->cur))
                return 0;                   /* if row does not exist, abort */
              row = psp->rows +size;        /* get the indexed row */
              if ((supp < row->min)     || (supp > row->cur))
                return 0;                   /* if counter does not exist, abort */
              return row->frqs[supp -row->min];     /* return the counter value */
              }  /* psp_getfrq() */
              
              /*--------------------------------------------------------------------*/
              
              static int resize (PATSPEC *psp, ITEM size, RSUPP supp)
              {                               /* --- resize the row array (sizes) */
              size_t n, i;                  /* new maximum size, loop variable */
              PSPROW *row;                  /* to access/reallocate the rows */
              RSUPP  min, max;              /* new minimum and maximum support */
              size_t *p;                    /* to reallocate the counter array */
              
              assert(psp                    /* check the function arguments */
              &&    (size >= psp->minsize) && (size <= psp->maxsize)
                       &&    (supp >= psp->minsupp) && (supp <= psp->maxsupp));
                       if (size > psp->max) {        /* if outside of size range */
              n = (psp->max > 0) ? (size_t)psp->max : 0;
                         n += (n > BLKSIZE) ? n >> 1 : BLKSIZE;
                         if (n < (size_t)size)         n = (size_t)size;
                         if (n > (size_t)psp->maxsize) n = (size_t)psp->maxsize;
                         row = (PSPROW*)realloc(psp->rows, (n+1) *sizeof(PSPROW));
                         if (!row) return psp->err = -1;   /* enlarge the row array */
              for (i = (size_t)psp->max; ++i <= n; )
                row[i] = empty;           /* initialize the new elements */
              psp->rows = row;            /* set the new array */
              psp->max  = (ITEM)n;        /* and its size */
                       }
                       row = psp->rows +size;        /* get the indexed row */
              if ((supp >= row->min) && (supp <= row->max))
                return 0;                   /* if support is in range, abort */
              if      (!row->frqs)               min = supp     -BLKSIZE;
              else if (supp > row->min)          min = row->min;
              else if (supp > row->min -BLKSIZE) min = row->min -BLKSIZE;
              else                               min = supp;
              if (min < psp->minsupp +BLKSIZE)   min = psp->minsupp;
              if      (!row->frqs)               max = supp     +BLKSIZE;
              else if (supp < row->max)          max = row->max;
              else if (supp < row->max +BLKSIZE) max = row->max +BLKSIZE;
              else                               max = supp;
              if (max > psp->maxsupp)            max = psp->maxsupp;
              if (size <= 0) min = max = supp; /* only one counter for size = 0 */
              n = (size_t)max -(size_t)min +1; /* compute the new array size */
              p = (size_t*)realloc(row->frqs, n *sizeof(size_t));
              if (!p) return psp->err = -1; /* enlarge the counter array */
              if      (!row->frqs)          /* if new array created */
              memset(p, 0, n *sizeof(size_t));
              else if (supp > row->max) {   /* if enlarged at the end */
              n = (size_t)row->max -(size_t)row->min +1;
                memset(p +n, 0, (size_t)(max -row->max) *sizeof(size_t)); }
              else if (supp < row->min) {   /* if enlarged at the front */
              n = (size_t)(row->max -row->min +1);
                memmove(p +(row->min -min), p, n *sizeof(size_t));
                memset (p, 0, (size_t)(row->min -min) *sizeof(size_t));
              }                             /* move the existing counters, */
              row->frqs = p;                /* initialize the new elements */
              row->min  = min;              /* and set the new array */
              row->max  = max;              /* and its range */
              return 0;                     /* return 'ok' */
              }  /* resize() */
              
              int psp_incfrq (PATSPEC *psp, ITEM size, RSUPP supp, size_t frq)
              {                               /* --- increase a counter value */
              PSPROW *row;                  /* to access the table row */
              
              assert(psp);                  /* check the function arguments */
              if ((size < psp->minsize) || (size > psp->maxsize)
                    ||  (supp < psp->minsupp) || (supp > psp->maxsupp))
                return 0;                   /* ignore values outside range */
              if (resize(psp, size, supp) < 0)
                return psp->err = -1;       /* enlarge table if necessary */
              if (size > psp->cur)          /* update the maximum size */
              psp->cur = size;            /* that is currently represented */
              row   = psp->rows +size;      /* get the indexed row and */
              if (supp > row->cur)          /* update the maximum support */
              row->cur = supp;            /* that is currently represented */
              supp -= row->min;             /* remove the support offset */
              if ((row->frqs[supp] <= 0) && (row->frqs[supp] +frq > 0))
                psp->sigcnt++;              /* count a new signature */
              row->frqs[supp] += frq;       /* update the signature frequency */
              row->sum        += frq;       /* update the sum for the size */
              psp->total      += frq;       /* as well as the total frequency */
              return 0;                     /* return 'ok' */
              }  /* psp_incfrq() */
              
              /*----------------------------------------------------------------------
              Type Definitions
              ----------------------------------------------------------------------*/
              typedef struct {                /* --- memory system state --- */
              void    **next;               /* next unassigned object */
              void    **curr;               /* current memory block */
              size_t  used;                 /* number of used objects */
              } MSSTATE;                      /* (memory system state) */
              
              typedef struct {                /* --- memory management system --- */
              size_t  size;                 /* size of an object (in void* units) */
              size_t  mbsz;                 /* size of a memory block */
              size_t  used;                 /* number of used objects */
              size_t  umax;                 /* maximum number of used objects */
              void    **free;               /* list of free objects */
              void    **next;               /* next unassigned object */
              void    **curr;               /* current memory block */
              void    **list;               /* list of allocated memory blocks */
              int     err;                  /* allocation error indicator */
              size_t  cap;                  /* stack capacity (maximum size) */
              size_t  top;                  /* top of stack (next free element) */
              MSSTATE *stack;               /* stack of memory system states */
              } MEMSYS;                       /* (memory management system) */
              
              /*----------------------------------------------------------------------
              Functions
              ----------------------------------------------------------------------*/
              MEMSYS*   ms_create (size_t size, size_t cnt);
              void      ms_delete (MEMSYS *ms);
              void      ms_clear  (MEMSYS *ms, int shrink);
              void*     ms_alloc  (MEMSYS *ms);
              void      ms_free   (MEMSYS *ms, void *obj);
              ptrdiff_t ms_push   (MEMSYS *ms);
              ptrdiff_t ms_pop    (MEMSYS *ms);
              int       ms_error  (MEMSYS *ms);
              int       ms_reset  (MEMSYS *ms);
              size_t    ms_used   (MEMSYS *ms);
              size_t    ms_umax   (MEMSYS *ms);
              
              /*----------------------------------------------------------------------
              Preprocessor Definitions
              ----------------------------------------------------------------------*/
#define ms_error(m)      ((m)->err)
#define ms_reset(m)      ((m)->err = 0)
#define ms_used(m)       ((m)->used)
#define ms_umax(m)       ((m)->umax)
              
              
              /*----------------------------------------------------------------------
              Type Definitions
----------------------------------------------------------------------*/
              
              typedef struct cmnode {         /* --- c/m prefix tree node --- */
              ITEM          item;           /* associated item (last item in set) */
              RSUPP         supp;           /* support of represented item set */
              struct cmnode *sibling;       /* successor node in sibling list */
              struct cmnode *children;      /* list of child nodes */
              } CMNODE;                       /* (c/m prefix tree node) */
              
              typedef struct {                /* --- c/m prefix tree --- */
              MEMSYS *mem;                  /* memory management system */
              ITEM   size;                  /* (maximum) number of items */
              int    dir;                   /* direction of item order */
              ITEM   item;                  /* associated prefix item */
              RSUPP  max;                   /* maximum support for prefix */
              CMNODE root;                  /* root node of the tree */
              int    keep[1];               /* flags for cmt_xproj() calls */
              } CMTREE;                       /* (c/m prefix tree) */
              
              typedef struct {                /* --- closed/maximal filter --- */
              int    dir;                   /* direction of item order */
              ITEM   size;                  /* maximum number of prefix trees */
              ITEM   cnt;                   /* current number of prefix trees */
              CMTREE *trees[1];             /* conditional prefix trees */
              } CLOMAX;                       /* (closed/maximal filter) */
              
              /*----------------------------------------------------------------------
              Prefix Tree Functions
              ----------------------------------------------------------------------*/
              CMTREE* cmt_create  (MEMSYS *mem, int dir, ITEM size);
              void    cmt_clear   (CMTREE *cmt);
              void    cmt_delete  (CMTREE *cmt, int delms);
              MEMSYS* cmt_memsys  (CMTREE *cmt);
              ITEM    cmt_cnt     (CMTREE *cmt);
              int     cmt_dir     (CMTREE *cmt);
              RSUPP   cmt_supp    (CMTREE *cmt);
              RSUPP   cmt_max     (CMTREE *cmt);
              int     cmt_valid   (CMTREE *cmt);
              size_t  cmt_nodecnt (CMTREE *pat);
              size_t  cmt_nodemax (CMTREE *pat);
              
              int     cmt_add     (CMTREE *cmt, const ITEM *items, ITEM n,
                                   RSUPP supp);
              RSUPP   cmt_get     (CMTREE *cmt, const ITEM *items, ITEM n);
              void    cmt_prune   (CMTREE *cmt, ITEM item);
              CMTREE* cmt_project (CMTREE *dst, CMTREE *src, ITEM item);
              CMTREE* cmt_xproj   (CMTREE *dst, CMTREE *src, ITEM item,
                                   const ITEM *keep, ITEM n);
              
              
              /*----------------------------------------------------------------------
              Closed/Maximal Filter Functions
              ----------------------------------------------------------------------*/
              CLOMAX* cm_create   (int dir, ITEM size);
              void    cm_delete   (CLOMAX *cm);
              ITEM    cm_cnt      (CLOMAX *cm);
              int     cm_dir      (CLOMAX *cm);
              RSUPP   cm_supp     (CLOMAX *cm);
              CMTREE* cm_tree     (CLOMAX *cm, ITEM i);
              
              int     cm_add      (CLOMAX *cm, ITEM item, RSUPP supp);
              int     cm_addnc    (CLOMAX *cm, ITEM item, RSUPP supp);
              void    cm_remove   (CLOMAX *cm, ITEM n);
              RSUPP   cm_tail     (CLOMAX *cm, const ITEM *items, ITEM n);
              int     cm_update   (CLOMAX *cm, const ITEM *items, ITEM n,
                                   RSUPP supp);
              
              /*----------------------------------------------------------------------
              Preprocessor Definitions
              ----------------------------------------------------------------------*/
#define cmt_memsys(t)      ((t)->mem)
#define cmt_cnt(t)         ((t)->cnt)
#define cmt_dir(t)         ((t)->dir)
#define cmt_supp(t)        ((t)->root.supp)
#define cmt_max(t)         ((t)->max)
#define cmt_valid(t)       ((t)->item >= -1)
#define cmt_nodecnt(t)     (ms_used((t)->mem))
#define cmt_nodemax(t)     (ms_umax((t)->mem))
              
#define cm_cnt(f)          ((f)->cnt)
#define cm_dir(f)          ((f)->dir)
#define cm_tree(f,i)       ((f)->trees[i])
              
              
              
              MEMSYS* ms_create (size_t size, size_t cnt)
              {                               /* --- create a memory system */
              MEMSYS *ms;                   /* created memory system */
              
              assert((cnt > 0) && (size > 0));   /* check the function arguments */
              ms = (MEMSYS*)malloc(sizeof(MEMSYS));
              if (!ms) return NULL;         /* create a memory management system */
              ms->size = (size+sizeof(void*)-1) /sizeof(void*);
              /* ms->size: the size of an object in void* units (rounded up) */
              ms->mbsz = cnt *ms->size +2;  /* 2: successor and predecessor */
              /* ms->mbsz: the size of a memory block in void* units */
              ms->free = ms->next = ms->curr = ms->list = NULL;
              ms->used = ms->umax = 0;      /* initialize the variables */
              ms->cap  = ms->top  = 0; ms->stack = NULL;
              ms->err  = 0;
              return ms;                    /* return the created memory system */
              }  /* ms_create() */
              
              /*--------------------------------------------------------------------*/
              
              void ms_delete (MEMSYS *ms)
              {                               /* --- delete a memory system */
              void **b;                     /* buffers for deallocation */
              
              assert(ms);                   /* check the function argument */
              for (b = ms->list; b; b = ms->list) {
                ms->list = (void**)*b; free(b); }
              while (ms->list) {            /* delete the list of blocks */
              b = ms->list; ms->list = (void**)*b; free(b); }
              if (ms->stack) free(ms->stack);  /* delete a possible stack */
              free(ms);                        /* and the base structure */
              }  /* ms_delete() */
              
              /*--------------------------------------------------------------------*/
              
              void ms_clear (MEMSYS *ms, int shrink)
              {                               /* --- clear a memory system */
              void **b;                     /* buffer for a memory block */
              
              if (!(b = ms->list))          /* if there are no memory blocks, */
              ms->next = ms->curr = NULL; /* clear the unassigned objects */
              else {                        /* if there are memory blocks, */
              if (shrink) {               /* if to shrink the memory block list */
              while (*b) { ms->list = (void**)*b; free(b); b = ms->list; }
              b[1] = NULL;              /* delete all but the last block */
              }                           /* and clear its successor pointer */
              ms->curr = b;               /* set the only memory block that */
              ms->next = b+2;             /* is left as the current block */
              }                             /* and set next unassigned object */
              ms->free = NULL;              /* clear the list of free objects */
              ms->used = ms->umax = 0;      /* there are no used objects and */
              ms->cap  = ms->top  = 0;      /* the memory state stack is empty */
              ms->err  = 0;                 /* clear the error indicator */
              }  /* ms_clear() */
              
              /*--------------------------------------------------------------------*/
              
              void* ms_alloc (MEMSYS *ms)
              {                               /* --- allocate an object */
              void **obj;                   /* allocated object */
              void **b;                     /* next/new memory block */
              
              assert(ms);                   /* check the function argument */
              if ((obj = ms->free)) {       /* if there is a free object, */
              ++ms->used;                 /* count the object as used, */
              ms->free = (void**)*(void**)obj;
              return obj;                 /* remove it from the free list */
              }                             /* and return it */
              /* The new value of ms->used cannot exceed ms->umax, because all  */
              /* objects on the free list have been in use before and thus have */
              /* already been counted for the maximum number of used objects.   */
              if      (ms->next)            /* if there are new objects, */
              obj = ms->next;             /* get the next new object */
              else if (((b = ms->curr)) && b[1]) {
                ms->curr = b = (void**)b[1];/* if there is a successor block */
              ms->next = obj = b+2; }     /* and get its first object */
              else {                        /* if there is no new object left */
              b = (void**)malloc(ms->mbsz *sizeof(void*));
                if (!b) { ms->err = -1; return NULL; }
                b[1] = NULL;                /* allocate a new memory block and */
              b[0] = ms->list;            /* add it at the end of the list */
              if (ms->list) ms->list[1] = b;
              ms->list = ms->curr = b;    /* make the block the current one */
              ms->next = obj      = b+2;  /* and get the first object */
              }                             /* in this memory block */
              ms->next += ms->size;         /* advance the object array position */
              if (ms->next >= ms->curr +ms->mbsz)
                ms->next = NULL;            /* check for end of object array */
              if (++ms->used > ms->umax)    /* count the allocated object */
              ms->umax = ms->used;        /* and update the maximum */
              return obj;                   /* return the retrieved object */
              }  /* ms_alloc() */
              
              /*--------------------------------------------------------------------*/
              
              void ms_free (MEMSYS *ms, void *obj)
              {                               /* --- deallocate an f.p. tree node */
              assert(ms && obj);            /* check the function arguments */
              assert(ms->used > 0);         /* check the number of used objects */
              *(void**)obj = ms->free;      /* insert the freed object */
              ms->free     = (void**)obj;   /* at the head of the free list */
              ms->used--;                   /* count the deallocated object */
              }  /* ms_free() */
              
              /*--------------------------------------------------------------------*/
              
              ptrdiff_t ms_push (MEMSYS *ms)
              {                               /* --- store the current state */
              size_t  n;                    /* new stack size */
              MSSTATE *s;                   /* buffer for reallocation */
              
              assert(ms && !ms->free);      /* check the function argument */
              if (ms->top >= ms->cap) {     /* if the state stack is full */
              n = ms->cap +((ms->cap > 32) ? ms->cap >> 1 : 32);
                s = (MSSTATE*)realloc(ms->stack, n *sizeof(MSSTATE));
                if (!s) return -1;          /* enlarge the state stack */
              ms->stack = s; ms->cap = n; /* and set the enlarged stack */
              }                             /* and its new size */
              s = ms->stack +ms->top;       /* get the next free stack element */
              s->next = ms->next;           /* not the current state */
              s->curr = ms->curr;           /* of the memory system */
              s->used = ms->used;
              return (ptrdiff_t)++ms->top;  /* return the new stack size */
              }  /* ms_push() */
              
              /*--------------------------------------------------------------------*/
              
              ptrdiff_t ms_pop (MEMSYS *ms)
              {                               /* --- retrieve the last state */
              MSSTATE *s;                   /* to access the top stack element */
              
              assert(ms && !ms->free);      /* check the function argument */
              if (ms->top <= 0) return -1;  /* check for an empty stack */
              s = ms->stack +(--ms->top);   /* get the top stack element */
              ms->next = s->next;           /* restore the last state */
              ms->curr = s->curr;           /* of the memory system */
              ms->used = s->used;
              return (ptrdiff_t)ms->top;    /* return the new stack size */
              }  /* ms_pop() */
              
#define pos(x,y)   ((x) < (y))  /* macros for item comparison */
#define neg(x,y)   ((x) > (y))  /* (ascending and descending) */
              
#define COPYERR   ((CMNODE*)-1) /* error indicator for xcopy */
              
              /*----------------------------------------------------------------------
Prefix Tree Functions
----------------------------------------------------------------------*/
              
              CMTREE* cmt_create (MEMSYS *mem, int dir, ITEM size)
              {                               /* --- create a c/m prefix tree */
              CMTREE *cmt;                  /* created c/m prefix tree */
              
              assert(size > 0);             /* check the function arguments */
              cmt = (CMTREE*)malloc(sizeof(CMTREE) +(size_t)(size-1) *sizeof(ITEM));
              if (!cmt) return NULL;        /* create a c/m prefix tree */
              cmt->mem  = (mem) ? mem : ms_create(sizeof(CMNODE), 4095);
              if (!cmt->mem) { free(cmt); return NULL; }
              cmt->size = size;             /* note item count and order direction*/
              cmt->dir  = (dir < 0) ? -1 : +1;
              cmt->item = -2;               /* mark the tree as empty/invalid */
              cmt->max  = -2;               /* (special item/support) */
              memset(cmt->keep, 0, (size_t)size *sizeof(ITEM));
              cmt->root.sibling = cmt->root.children = NULL;
              cmt->root.item    = -1;       /* initialize the root node */
              cmt->root.supp    =  0;       /* (no item, no other nodes) */
              return cmt;                   /* return the created prefix tree */
              }  /* cmt_create() */
              
              /*--------------------------------------------------------------------*/
              
              void cmt_clear (CMTREE *cmt)
              {                               /* --- clear a c/m prefix tree */
              assert(cmt);                  /* check the function argument */
              ms_clear(cmt->mem, 0);        /* clear memory management system */
              cmt->max  = -2;               /* prefix tree is now empty/invalid */
              cmt->item = -2;               /* (special support/item) */
              cmt->root.sibling = cmt->root.children = NULL;
              cmt->root.supp    =  0;       /* reinitialize the root node */
              }  /* cmt_clear() */
              
              /*--------------------------------------------------------------------*/
              
              void cmt_delete (CMTREE *cmt, int delms)
              {                               /* --- delete a c/m prefix tree */
              assert(cmt);                  /* check the function arguments */
              if      (delms >  0) ms_delete(cmt->mem);
              else if (delms >= 0) ms_clear (cmt->mem, 1);
              free(cmt);                    /* delete memory system or nodes */
              }  /* cmt_delete() */           /* and delete the base structure */
              
              /*--------------------------------------------------------------------*/
              
              int cmt_add (CMTREE *cmt, const ITEM *items, ITEM n, RSUPP supp)
              {                               /* --- add an item set to a c/m tree */
              ITEM   i;                     /* buffer for an item */
              CMNODE **p;                   /* pointer to insertion position */
              CMNODE *node;                 /* to insert new nodes */
              
              assert(cmt                    /* check the function arguments */
              &&    (items || (n <= 0)) && (supp >= 0));
              if (supp > cmt->max)          /* update maximum item set support */
              cmt->max = supp;            /* (prefix tree is now valid) */
              /* In principle, it has to be checked whether the given item set   */
              /* contains at least one item and whether the first item coincides */
              /* with the item associated with the tree (if there is such item). */
              /* Otherwise cmt->max may not be updated with the support.         */
              node = &cmt->root;            /* start at the root node */
              do {                          /* traverse the items of the set */
              if (supp > node->supp)      /* adapt the node support */
              node->supp = supp;        /* (root represents empty set) */
              if (--n < 0) return 0;      /* if all items are processed, abort */
              i = *items++;               /* get the next item in the set and */
              p = &node->children;        /* traverse the list of children */
              if (cmt->dir < 0) while (*p && ((*p)->item > i)) p = &(*p)->sibling;
              else              while (*p && ((*p)->item < i)) p = &(*p)->sibling;
              node = *p;                  /* find the item/insertion position */
              } while (node && (node->item == i));
              node = (CMNODE*)ms_alloc(cmt->mem);
              if (!node) return -1;         /* create a new prefix tree node */
              node->supp    = supp;         /* store support of the item set */
              node->item    = i;            /* and the current/last item */
              node->sibling = *p;           /* insert the created node */
              *p = node;                    /* into the sibling list */
              while (--n >= 0) {            /* traverse the rest of the items */
              node = node->children = (CMNODE*)ms_alloc(cmt->mem);
                if (!node) return -1;       /* create a new prefix tree node */
              node->supp    = supp;       /* store support of the item set */
              node->item    = *items++;   /* and the current/last item */
              node->sibling = NULL;       /* there are no siblings yet */
              }
              node->children = NULL;        /* last created node is a leaf */
              return 0;                     /* return 'ok' */
              }  /* cmt_add() */
              
              /*--------------------------------------------------------------------*/
              
              RSUPP cmt_get (CMTREE *cmt, const ITEM *items, ITEM n)
              {                               /* --- get support of an item set */
              ITEM   i;                     /* buffer for an item */
              CMNODE *p;                    /* to traverse the nodes */
              
              assert(cmt && (items || (n <= 0))); /* check function arguments */
              p = &cmt->root;               /* start search at the root node */
              while (--n >= 0) {            /* traverse the items of the set */
              i = *items++;               /* try to find a child node */
              p = p->children;            /* with the next item in the set */
              if (cmt->dir < 0) while (p && (p->item > i)) p = p->sibling;
              else              while (p && (p->item < i)) p = p->sibling;
              if (!p || (p->item != i))   /* if a node with the next item */
              return -1;                /* does not exist in the tree, */
              }                             /* abort the search with failure */
              return p->supp;               /* return support of the item set */
              }  /* cmt_get() */
              
              /*--------------------------------------------------------------------*/
              
#define MERGE(dir)                                                                                 \
              static CMNODE* merge_##dir (CMNODE *s1, CMNODE *s2, MEMSYS *mem)                     \
              {                               /* --- merge two node list */                        \
              CMNODE *out, **end, *p;       /* output node list and end pointer */                 \
                                                                                                   \
              assert(mem);                  /* check the function arguments */                     \
              if (!s1) return s2;           /* if there is only one node list, */                  \
              if (!s2) return s1;           /* simply return the other list */                     \
              end = &out;                   /* start the output list */                            \
              while (1) {                   /* node list merge loop */                             \
              if      (dir(s1->item, s2->item)) {                                                  \
                *end = s1; end = &s1->sibling; s1 = *end; if (!s1) break; }                        \
              else if (dir(s2->item, s1->item)) {                                                  \
                *end = s2; end = &s2->sibling; s2 = *end; if (!s2) break; }                        \
              else {                      /* copy nodes with singular items */                     \
              s1->children = merge_##dir(s1->children, s2->children, mem);                         \
                if (s1->supp < s2->supp)  /* merge the children recursively */                     \
              s1->supp = s2->supp;    /* and update the node support */                            \
              p    = s2; s2  =  s2->sibling; ms_free(mem, p);                                      \
              *end = s1; end = &s1->sibling; s1 = *end;                                            \
              if (!s1 || !s2) break;    /* move node from the first source */                      \
              }                           /* to the output and delete the one */                   \
              }                             /* from the second source */                           \
              *end = (s1) ? s1 : s2;        /* append the remaining nodes */                       \
              return out;                   /* return the merged prefix tree */                    \
              }  /* merge() */
            
            /*--------------------------------------------------------------------*/
            
            MERGE(pos)                      /* function for ascending  item order */
            MERGE(neg)                      /* function for descending item order */
            
            /*--------------------------------------------------------------------*/
            
#define PRUNE(dir)                                                                               \
            static CMNODE* prune_##dir (CMNODE *node, ITEM item, MEMSYS *mem)                    \
            {                               /* --- prune a c/m prefix tree */                    \
            CMNODE *p, *b = NULL;         /* buffer for merged subtrees */                       \
                                                                                                 \
            assert(mem);                  /* check the function arguments */                     \
            while (node && dir(node->item, item)) {                                              \
              node->children =            /* prune children of current node */                   \
            p = prune_##dir(node->children, item, mem);                                          \
              if (p) b = (!b) ? p : merge_##dir(b, p, mem);                                      \
              p    = node;                /* merge remaining children with */                    \
            node = node->sibling;       /* the already collected subtrees */                     \
            ms_free(mem, p);            /* and delete the processed node */                      \
            }                             /* fimally merge with rem. nodes */                    \
            return (!node) ? b : (!b) ? node : merge_##dir(b, node, mem);                        \
            }  /* prune() */
          
          /*--------------------------------------------------------------------*/
          
          PRUNE(pos)                      /* function for ascending  item order */
          PRUNE(neg)                      /* function for descending item order */
          
          /*--------------------------------------------------------------------*/
          
          void cmt_prune (CMTREE *cmt, ITEM item)
          {                               /* --- prune a c/m prefix tree */
          CMNODE *p;                    /* to access the tree nodes */
          
          assert(cmt && (item >= 0));   /* check the function arguments */
          cmt->item = item;             /* note the item pruned with and */
          p = &cmt->root;               /* prune up to, but not item itself */
          p = p->children = (cmt->dir < 0)
            ? prune_neg(p->children, item, cmt->mem)
              : prune_pos(p->children, item, cmt->mem);
          cmt->max = (p && (p->item == item)) ? p->supp : -1;
          }  /* cmt_prune() */            /* set the maximal item set support */
          
          /*--------------------------------------------------------------------*/
          
          static CMNODE* copy2 (const CMNODE *src, MEMSYS *mem)
          {                               /* --- copy a c/m subtree */
          CMNODE *dst, *node;           /* created copy of the node list */
          CMNODE **end = &dst;          /* end of the created copy */
          CMNODE *c;                    /* buffer for copied children */
          
          assert(src && mem);           /* check the function arguments */
          do {                          /* sibling copying loop */
          *end = node = (CMNODE*)ms_alloc(mem);
            if (!node) return NULL;     /* create a copy for each node */
          node->item = src->item;     /* copy the item  */
          node->supp = src->supp;     /* and its support */
          c = src->children;          /* if there are children, copy them */
          if (c && !(c = copy2(c, mem))) return NULL;
          node->children = c;         /* store the (copied) children */
          end = &node->sibling;       /* get the new list end */
          src = src->sibling;         /* (to append the next node) */
          } while (src);                /* check for another sibling */
          *end = NULL;                  /* terminate the copied list */
          return dst;                   /* return the created copy */
          }  /* copy2() */
          
          /*--------------------------------------------------------------------*/
          
          static void* delclr (CMTREE *cmt, int del)
          {                               /* --- delete or clear a prefix tree */
          if (del) cmt_delete(cmt, 1);  /* if deletion flag is set, delete, */
          else     cmt_clear (cmt);     /* otherwise only clear the tree */
          return NULL;                  /* return an error indicator */
          }  /* delclr() */
          
          /*--------------------------------------------------------------------*/
          
          CMTREE* cmt_project (CMTREE *dst, CMTREE *src, ITEM item)
          {                               /* --- project a c/m prefix tree */
          CMTREE *arg = dst;            /* buffer for destination argument */
          CMNODE *p;                    /* to traverse the tree nodes */
          
          assert(src && (item >= 0)     /* check the function arguments */
          &&    (!dst || (dst->dir == src->dir)));
          if (!dst) dst = cmt_create(NULL, src->dir, src->size-1);
          if (!dst) return NULL;        /* create/reinit. destination tree */
          src->item = item;             /* note the projection item */
          dst->item = -1; dst->max = src->max = -1;
          dst->root.supp = 0;           /* clear maximum item set support */
          assert(!dst->root.children);  /* check for an empty destination */
          p = &src->root;               /* if the prefix tree is empty, */
          if (!p->children) return dst; /* directly return the destination */
          p = p->children = (src->dir < 0) /* prune top level up to the item */
          ? prune_neg(p->children, item, src->mem)
            : prune_pos(p->children, item, src->mem);
          if (!p || (p->item != item))  /* if projection item is missing, */
          return dst;                 /* directly return the destination */
          dst->root.supp = src->max = p->supp;   /* set projection support */
          if (p->children) {            /* if projection item has children */
          dst->root.children = p = copy2(p->children, dst->mem);
            if (!p) return delclr(dst, !arg);
          }                             /* copy the child nodes to the dest. */
          p = &src->root;               /* prune the projection item */
          p->children = (src->dir < 0)  /* from the source prefix tree */
          ? prune_neg(p->children, item-1, src->mem)
            : prune_pos(p->children, item+1, src->mem);
          return dst;                   /* return the destination tree */
          }  /* cmt_project() */
          
          /*--------------------------------------------------------------------*/
          
#define XCOPY(dir)                                                                             \
          static CMNODE* xcopy_##dir (const CMNODE *src, MEMSYS *mem,                          \
                                      const int *keep)                                         \
          {                               /* --- copy a c/m subtree */                         \
          CMNODE *dst, *node;           /* created copy of the node list */                    \
          CMNODE **end = &dst;          /* end of the created copy */                          \
          CMNODE *c, *b = NULL;         /* buffer for copied children */                       \
                                                                                               \
          assert(src && mem);           /* check the function arguments */                     \
          do {                          /* sibling copying loop */                             \
          c = src->children;          /* if there children */                                  \
          if (c && ((c = xcopy_##dir(c, mem, keep)) == COPYERR))                               \
            return COPYERR;           /* recursively copy the children */                      \
          if (keep[src->item]) {      /* if to copy the node itself */                         \
          *end = node = (CMNODE*)ms_alloc(mem);                                                \
          if (!node) return COPYERR;/* create a copy of the node */                            \
          node->item = src->item;   /* copy the item and the support */                        \
          node->supp = src->supp;   /* into the created copy */                                \
          node->children = c;       /* set the (copied) children */                            \
          end = &node->sibling; }   /* get the new list end */                                 \
          else if (c)                 /* if there are copied children */                       \
          b = (b) ? merge_##dir(b, c, mem) : c;                                                \
          src = src->sibling;         /* merge them into a buffer */                           \
          } while (src);                /* check for another sibling */                        \
          *end = NULL;                  /* terminate the copied list */                        \
          return (!b) ? dst : (!dst) ? b : merge_##dir(dst, b, mem);                           \
          }  /* xcopy() */               /* return the created copy */
        
        /*--------------------------------------------------------------------*/
        
        XCOPY(pos)                      /* function for ascending  item order */
        XCOPY(neg)                      /* function for descending item order */
        
        /*--------------------------------------------------------------------*/
        
        CMTREE* cmt_xproj (CMTREE *dst, CMTREE *src, ITEM item,
                           const ITEM *keep, ITEM n)
        {                               /* --- project a c/m prefix tree */
        ITEM   i;                     /* loop variable */
        CMTREE *arg = dst;            /* buffer for destination argument */
        CMNODE *p;                    /* to traverse the tree nodes */
        
        assert(src && (item >= 0)     /* check the function arguments */
        &&    (!dst || (dst->dir == src->dir)) && keep);
        if (!dst) dst = cmt_create(NULL, src->dir, src->size-1);
        if (!dst) return NULL;        /* create the destination tree */
        src->item = item;             /* note the projection item */
        dst->item = -1; dst->max = src->max = -1;
        dst->root.supp = 0;           /* clear maximum item set support */
        assert(!dst->root.children);  /* check for an empty destination */
        p = &src->root;               /* prune top level up to the item */
        p = p->children = (src->dir < 0)
          ? prune_neg(p->children, item, src->mem)
            : prune_pos(p->children, item, src->mem);
        if (!p || (p->item != item))  /* if projection item is missing, */
        return dst;                 /* directly return the destination */
        dst->root.supp = src->max = p->supp;   /* set projection support */
        if (p->children) {            /* if projection item has children */
        for (i = n; --i >= 0; )     /* traverse the items to keep and */
        dst->keep[keep[i]] = 1;   /* set the corresponding flags */
        p = (dst->dir < 0)          /* copy branch of projection item */
        ? xcopy_neg(p->children, dst->mem, dst->keep)
          : xcopy_pos(p->children, dst->mem, dst->keep);
        for (i = n; --i >= 0; )     /* traverse the items to keep and */
        dst->keep[keep[i]] = 0;   /* clear the corresponding flags */
        if (p == COPYERR) return delclr(dst, !arg);
        dst->root.children = p;     /* store the copied branch */
        }
        p = &src->root;               /* prune the projection item */
        p->children = (src->dir < 0)  /* from the source prefix tree */
        ? prune_neg(p->children, item-1, src->mem)
          : prune_pos(p->children, item+1, src->mem);
        return dst;                   /* return the destination tree */
        }  /* cmt_xproj() */
        
        /*----------------------------------------------------------------------
          Closed/Maximal Filter Functions
          ----------------------------------------------------------------------*/
        
        CLOMAX* cm_create (int dir, ITEM size)
        {                               /* --- create a c/m filter object */
        CLOMAX *cm;                   /* created closed/maximal filter */
        CMTREE *t;                    /* created root prefix tree */
        
        assert(size > 0);             /* check the function arguments */
        cm = (CLOMAX*)calloc(1, sizeof(CLOMAX) +(size_t)size*sizeof(CMTREE*));
        if (!cm) return NULL;         /* create a closed/maximal filter */
        cm->size = size;              /* and initialize its fields */
        cm->dir  = (dir < 0) ? -1 : +1;
        cm->cnt  = 0;                 /* there is no prefix yet */
        cm->trees[0] = t = cmt_create(NULL, dir, size);
        if (!t) { cm_delete(cm); return NULL; }
        cmt_add(t, NULL, 0, 0);       /* create and init. a root tree */
        t->item = -1;                 /* mark the root tree as valid */
        return cm;                    /* return the created c/m filter */
        }  /* cm_create() */
        
        /*--------------------------------------------------------------------*/
        
        void cm_delete (CLOMAX *cm)
        {                               /* --- delete a c/m filter object */
        ITEM i;                       /* loop variable */
        
        assert(cm);                   /* check the function argument */
        for (i = 0; cm->trees[i]; i++)/* traverse the c/m prefix trees */
        cmt_delete(cm->trees[i],1); /* and delete them */
        free(cm);                     /* delete the base structure */
        }  /* cm_delete() */
        
        /*--------------------------------------------------------------------*/
        
        RSUPP cm_supp (CLOMAX *cm)
        {                               /* --- get support of current prefix */
        assert(cm);                   /* check the function argument */
        return (cm->cnt > 0) ? cmt_max (cm->trees[cm->cnt-1])
          : cmt_supp(cm->trees[0]);
        }  /* cm_supp() */
        
        /*--------------------------------------------------------------------*/
        
        int cm_add (CLOMAX *cm, ITEM item, RSUPP supp)
        {                               /* --- add an item to the prefix */
        CMTREE *t, **p;               /* to access the c/m prefix trees */
        
        assert(cm                     /* check the function arguments */
        &&    (item >= 0) && (item < cm->size));
        p = cm->trees +cm->cnt;       /* get the current prefix tree */
        if (!*p || !cmt_valid(*p)) {  /* if there is no (valid) tree, */
        t = p[-1];                  /* get the parent tree (source), */
        t = cmt_project(*p, t, t->item);
        if (!t) return -1;          /* project it to the corresponding */
        *p = t;                     /* extension item (end of prefix), */
        }                             /* and store the created projection */
        cmt_prune(t = *p, item);      /* prune the current prefix tree */
        if (t->max >= supp) return 0; /* check the support of the prefix */
        ++cm->cnt; return 1;          /* count the added item*/
        }  /* cm_add() */
        
        /*--------------------------------------------------------------------*/
        
        int cm_addnc (CLOMAX *cm, ITEM item, RSUPP supp)
        {                               /* --- add an item to the prefix */
        CMTREE *t, **p;               /* to access the c/m prefix trees */
        
        assert(cm                     /* check the function arguments */
        &&    (item >= 0) && (item < cm->size));
        p = cm->trees +cm->cnt;       /* get the current prefix tree */
        if (!*p || !cmt_valid(*p)) {  /* if there is no (valid) tree, */
        t = p[-1];                  /* get the parent tree (source), */
        t = cmt_project(*p, t, t->item);
        if (!t) return -1;          /* project it to the corresponding */
        *p = t;                     /* extension item (end of prefix), */
        }                             /* and store the created projection */
        cmt_prune(*p, item);          /* prune the current prefix tree */
        ++cm->cnt; return 1;          /* count the added item */
        }  /* cm_addnc() */
        
        /* In contrast to cm_add(), the function cm_addnc() does not check */
        /* whether the extended prefix possesses a perfect extension.      */
        
        /*--------------------------------------------------------------------*/
        
        void cm_remove (CLOMAX *cm, ITEM n)
        {                               /* --- remove items from the prefix */
        assert(cm && (n >= 0));       /* check the function arguments */
        for (n = (n < cm->cnt) ? cm->cnt -n : 0; cm->cnt > n; cm->cnt--)
          if (cm->trees[cm->cnt]) cmt_clear(cm->trees[cm->cnt]);
        }  /* cm_remove() */             /* traverse and clear the trees */
        
        /*--------------------------------------------------------------------*/
        
        RSUPP cm_tail (CLOMAX *cm, const ITEM *items, ITEM n)
        {                               /* --- prune with the tail items */
        RSUPP  s;                     /* support of the tail item set */
        CMTREE *t, **p;               /* to access the c/m prefix trees */
        
        assert(cm && (items || (n <= 0))); /* check the function arguments */
        if (n == 0) return 1;         /* empty tails can be ignored */
        if (cm->cnt <= 0) return 0;   /* check for a non-empty prefix */
        p = cm->trees +cm->cnt;       /* get the current prefix tree(s)  */
        t = p[-1];                    /* (source and destination) */
        t = cmt_xproj(*p, t, t->item, items, n);
        if (!t) return -1;            /* project parent with the tail items */
        *p = t;                       /* set the created projection */
        if (n < 0) return 0;          /* if not to check support, abort */
        s = cmt_get(t, items, n);     /* get support of given tail item set */
        return (s > 0) ? s : 0;       /* and check whether it is positive */
        }  /* cm_tail() */
        
        /*--------------------------------------------------------------------*/
        
        int cm_update (CLOMAX *cm, const ITEM *items, ITEM n, RSUPP supp)
        {                               /* --- update filter with found set */
        ITEM   i;                     /* loop variable */
        CMTREE *t;                    /* to traverse the prefix trees */
        
        assert(cm                     /* check function arguments */
        &&    (items || (n <= 0)) && (supp >= 0));
        for (i = 0; i < cm->cnt; i++) {
          t = cm->trees[i];           /* traverse the c/m prefix trees */
        while (*items != t->item) { ++items; --n; }
        if (cmt_add(t, ++items, --n, supp) < 0)
          return -1;                /* add the proper suffix of the set */
        }                             /* to report to the c/m prefix trees */
        return 0;                     /* return 'ok' */
        }  /* cm_update() */
        
        
#ifndef UCHAR
#define UCHAR unsigned char     /* abbreviation */
#endif
        
        /*----------------------------------------------------------------------
Preprocessor Definitions
----------------------------------------------------------------------*/
        /* --- targets (for isr_settarg()) --- */
#define ISR_SETS      0x0000    /* report all frequent item sets */
#define ISR_ALL       0x0000    /* report all frequent item sets */
#define ISR_FREQ      0x0000    /* report all frequent item sets */
#define ISR_FREQUENT  0x0000    /* report all frequent item sets */
#define ISR_CLOSED    0x0001    /* report only closed  item sets */
#define ISR_MAXIMAL   0x0002    /* report only maximal item sets */
#define ISR_GENERAS   0x0004    /* report only generators */
#define ISR_RULES     0x0008    /* report association rules */
        
        /* --- modes (for isr_settarg()) --- */
#define ISR_NOFILTER  0x0010    /* do not use internal filtering */
#define ISR_NOEXPAND  0x0020    /* do not expand perfect extensions */
#define ISR_SORT      0x0040    /* generator filtering needs sorting */
#define ISR_SEQUENCE  0x0080    /* allow for sequences (repeat items) */
        
        
        /*----------------------------------------------------------------------
        Type Definitions
----------------------------------------------------------------------*/
        struct isreport;                /* --- an item set eval. function --- */
        typedef double ISEVALFN (struct isreport *rep, void *data);
        typedef void   ISREPOFN (struct isreport *rep, void *data);
        typedef void   ISRULEFN (struct isreport *rep, void *data,
                                 ITEM item, RSUPP body, RSUPP head);
        
        typedef struct isreport {       /* --- an item set reporter --- */
        ITEMBASE   *base;             /* underlying item base */
        int        target;            /* target type (e.g. ISR_CLOSED) */
        int        mode;              /* reporting mode (e.g. ISR_SORT) */
        ITEM       zmin;              /* minimum number of items in set */
        ITEM       zmax;              /* maximum number of items in set */
        ITEM       xmax;              /* maximum number for isr_xable() */
        ITEM       size;              /* size of the item array "items" */
        RSUPP      smin;              /* minimum support of an item set */
        RSUPP      smax;              /* maximum support of an item set */
        RSUPP      *border;           /* item set filtering border */
        ITEM       bdrcnt;            /* number of used border entries */
        ITEM       bdrsize;           /* size of filtering border */
        ITEM       cnt;               /* current number of items in set */
        ITEM       pfx;               /* number of items in valid prefix */
        ITEM       *pxpp;             /* number of perfect exts. per prefix */
        ITEM       *pexs;             /* perfect extension items */
        ITEM       *items;            /* current item set (array of items) */
        RSUPP      *supps;            /* (prefix) item sets support values */
        double     *wgts;             /* (prefix) item sets weights */
        double     *ldps;             /* binary logarithms of item probs. */
        CLOMAX     *clomax;           /* closed/maximal item set filter */
        SYMTAB     *gentab;           /* generator      item set filter */
        RSUPP      sto;               /* max. superset support for storing */
        int        dir;               /* direction of item order in clomax */
        ITEM       *iset;             /* additional buffer for an item set */
        ISEVALFN   *evalfn;           /* additional evaluation function */
        void       *evaldat;          /* additional evaluation data */
        int        evaldir;           /* direction of evaluation */
        double     evalthh;           /* threshold of evaluation */
        double     eval;              /* additional evaluation value */
        ISREPOFN   *repofn;           /* item set reporting function */
        void       *repodat;          /* item set reporting data */
        ISRULEFN   *rulefn;           /* assoc. rule reporting function */
        void       *ruledat;          /* assoc. rule reporting data */
        int        scan;              /* flag for scanable item output */
        CCHAR      *str;              /* buffer for format strings */
        CCHAR      *hdr;              /* record header for output */
        CCHAR      *sep;              /* item separator for output */
        CCHAR      *imp;              /* implication sign for rule output */
        CCHAR      *iwf;              /* format for item weight output */
        CCHAR      *info;             /* format for information output */
        CCHAR      **inames;          /* (formatted) item names */
        size_t     nmax;              /* maximum of the item name sizes */
        size_t     nsum;              /* sum of the item name sizes */
        size_t     repcnt;            /* number of reported item sets */
        size_t     *stats;            /* reported item sets per set size */
        PATSPEC    *psp;              /* an (optional) pattern spectrum */
        char       **ints;            /* preformatted integer numbers */
        TID        imin;              /* smallest pre-formatted integer */
        TID        imax;              /* largest  pre-formatted integer */
        FILE       *file;             /* output file to write to */
        CCHAR      *name;             /* name of item set output file */
        char       *buf;              /* write buffer for output */
        char       *next;             /* next character position to write */
        char       *end;              /* end of the write buffer */
        FILE       *tidfile;          /* output file for transaction ids */
        CCHAR      *tidname;          /* name of tid output file */
        char       *tidbuf;           /* write buffer for output */
        char       *tidnxt;           /* next character position to write */
        char       *tidend;           /* end of the write buffer */
        ITEM       *occs;             /* array  of item occurrences */
        TID        *tids;             /* array  of transaction ids */
        TID        tidcnt;            /* number of transaction ids */
        TID        tracnt;            /* total number of transactions */
        ITEM       miscnt;            /* accepted number of missing items */
        int        fast;              /* whether fast output is possible */
        int        fosize;            /* size of set info. for fastout() */
        char       foinfo[64];        /* item set info.    for fastout() */
        char       *out;              /* output buffer for sets/rules */
        char       *pos[1];           /* append positions in output buffer */
        } ISREPORT;                     /* (item set reporter) */
        
        /*----------------------------------------------------------------------
        Functions
        ----------------------------------------------------------------------*/
        ISREPORT* isr_createx  (ITEMBASE *base, ITEM max);
        int       isr_delete   (ISREPORT *rep, int delis);
        ITEMBASE* isr_base     (ISREPORT *rep);
        
        int       isr_settarg  (ISREPORT *rep,
                                int target, int mode, int dir);
        void      isr_setsupp  (ISREPORT *rep, RSUPP smin, RSUPP smax);
        void      isr_setsize  (ISREPORT *rep, ITEM  zmin, ITEM  zmax);
        
        void      isr_seteval  (ISREPORT *rep, ISEVALFN evalfn,
                                void *data, int dir, double thresh);
        void      isr_setrepo  (ISREPORT *rep, ISREPOFN repofn,
                                void *data);
        void      isr_setrule  (ISREPORT *rep, ISRULEFN rulefn,
                                void *data);
        int       isr_prefmt   (ISREPORT *rep, TID min, TID max);
        
        int       isr_close    (ISREPORT *rep);
        CCHAR*    isr_name     (ISREPORT *rep);
        
        int       isr_tidclose (ISREPORT *rep);
        
        int       isr_setup    (ISREPORT *rep);
        
        int       isr_add      (ISREPORT *rep, ITEM item, RSUPP supp);
        int       isr_addpex   (ISREPORT *rep, ITEM item);
        void      isr_addpexpk (ISREPORT *rep, ITEM bits);
        int       isr_uses     (ISREPORT *rep, ITEM item);
        void      isr_remove   (ISREPORT *rep, ITEM n);
        int       isr_xable    (ISREPORT *rep, ITEM n);
        
        ITEM      isr_cnt      (ISREPORT *rep);
        ITEM      isr_item     (ISREPORT *rep);
        ITEM      isr_itemx    (ISREPORT *rep, ITEM index);
        RSUPP     isr_supp     (ISREPORT *rep);
        RSUPP     isr_suppx    (ISREPORT *rep, ITEM index);
        double    isr_eval     (ISREPORT *rep);
        
        ITEM      isr_pexcnt   (ISREPORT *rep);
        
        double    isr_logrto   (ISREPORT *rep, void *data);
        
        void*     isr_itemobj  (ISREPORT *rep, ITEM item);
        int       isr_report   (ISREPORT *rep);
        
        size_t    isr_repcnt   (ISREPORT *rep);
        int       isr_addpsp   (ISREPORT *rep, PATSPEC *psp);
        PATSPEC*  isr_getpsp   (ISREPORT *rep);
        int       isr_intout   (ISREPORT *rep, diff_t num);
        int       isr_numout   (ISREPORT *rep, double num, int digits);
        int       isr_sinfo    (ISREPORT *rep, RSUPP supp, double wgt,
                                double eval);
        int       isr_tail     (ISREPORT *rep, const ITEM *items,ITEM n);
        
        /*----------------------------------------------------------------------
        Preprocessor Definitions
        ----------------------------------------------------------------------*/
#define isr_base(r)       ((r)->base)
#define isr_name(r)       ((r)->name)
        
#define isr_uses(r,i)     ((r)->pxpp[i] < 0)
#define isr_xable(r,n)    ((r)->cnt+(n) <= (r)->xmax)
        
#define isr_cnt(r)        ((r)->cnt)
#define isr_item(r)       ((r)->items[(r)->cnt -1])
#define isr_itemx(r,i)    ((r)->items[i])
#define isr_supp(r)       ((r)->supps[(r)->cnt])
#define isr_suppx(r,i)    ((r)->supps[i])
#define isr_eval(r)       ((r)->eval)
        
#define isr_pexcnt(r)     ((ITEM)((r)->items -(r)->pexs))
        
#define isr_itemobj(r,i)  ib_obj((r)->base, i)
#define isr_basename(r,i) ib_name((r)->base, i)
        
#define isr_repcnt(r)     ((r)->repcnt)
#define isr_getpsp(r)     ((r)->psp)
#define isr_tail(r,i,n)   (cm_tail((r)->clomax, i, n) > 0 ? 1 : 0)
        
        /*----------------------------------------------------------------------
        Preprocessor Definitions
----------------------------------------------------------------------*/
#define BS_BORDER      32       /* block size for filtering border */
#define BS_WRITE    (64*1024)   /* size of internal write buffer */
#define BS_INT         48       /* buffer size for integer output */
#define BS_FLOAT       96       /* buffer size for float   output */
#define LN_2        0.69314718055994530942  /* ln(2) */
        
        /*----------------------------------------------------------------------
        Constants
----------------------------------------------------------------------*/
        static const double pows[] = {  /* several powers of ten */
        1e-02, 1e-01,                 /* for floating point number output */
        1e+00, 1e+01, 1e+02, 1e+03, 1e+04, 1e+05, 1e+06, 1e+07,
        1e+08, 1e+09, 1e+10, 1e+11, 1e+12, 1e+13, 1e+14, 1e+15,
        1e+16, 1e+17, 1e+18, 1e+19, 1e+20, 1e+21, 1e+22, 1e+23,
        1e+24, 1e+25, 1e+26, 1e+27, 1e+28, 1e+29, 1e+30, 1e+31,
        1e+32, 1e+33 };
        
        /*----------------------------------------------------------------------
        Basic Output Functions
        ----------------------------------------------------------------------*/
        
        static void fastchk (ISREPORT *rep)
        {                               /* --- check for fast output mode */
        if (rep->border               /* if there is a filtering border */
        ||  rep->repofn               /* or a report function */
        ||  rep->evalfn               /* or an evaluation function */
        ||  rep->tidfile)             /* or trans ids. are to be written, */
        rep->fast =  0;             /* standard output has to be used */
        else if (!rep->file)          /* if no output (and no filtering), */
        rep->fast = -1;             /* only count the item sets */
        else {                        /* if only an output file is written */
        rep->fast = ((rep->zmin <= 1) && (rep->zmax >= ITEM_MAX)
                       && ((strcmp(rep->info, " (%a)") == 0)
                             ||  (strcmp(rep->info, " (%d)") == 0))
                             &&  (strcmp(rep->hdr,  "")      == 0)
                             &&  (strcmp(rep->sep,  " ")     == 0)) ? +1 : 0;
        }                             /* check standard reporting settings */
        }  /* fastchk() */
        
        /*--------------------------------------------------------------------*/
        
        static int getsd (const char *s, const char **end)
        {                               /* --- get number of signif. digits */
        int k = 6;                    /* number of significant digits */
        
        assert(s && end);             /* check the function arguments */
        if ((*s >= '0') && (*s <= '9')) {
          k = *s++ -'0';              /* get the first digit */
        if ((*s >= '0') && (*s <= '9'))
          k = 10 *k +*s++ -'0';     /* get a possible second digit and */
        }                             /* compute the number of digits */
        if (k > 32) k = 32;           /* limit   the number of digits */
        *end = s; return k;           /* return  the number of digits */
        }  /* getsd() */
        
        /*--------------------------------------------------------------------*/
        
        
#define isr_finish(r)  isr_flush(r)
        static void isr_flush (ISREPORT *rep)
        {                               /* --- flush the output buffer */
        assert(rep);                  /* check the function arguments */
        fwrite(rep->buf, sizeof(char),(size_t)(rep->next-rep->buf),rep->file);
        rep->next = rep->buf;         /* write the output buffer */
        /* in debug mode */
        fflush(rep->file);            /* flush the output buffer */
        /* after every flush operation */
        }  /* isr_flush() */
        
        /*--------------------------------------------------------------------*/
        
        static void isr_putc (ISREPORT *rep, int c)
        {                               /* --- write a single character */
        assert(rep);                  /* check the function arguments */
        if (rep->next >= rep->end)    /* if the output buffer is full, */
        isr_flush(rep);             /* flush it (write it to the file) */
        *rep->next++ = (char)c;       /* store the given character */
        }  /* isr_putc() */
        
        /*--------------------------------------------------------------------*/
        
        static int isr_puts (ISREPORT *rep, const char *s)
        {                               /* --- write a character string */
        const char *t;                /* to traverse the characters string */
        
        assert(rep);                  /* check the function arguments */
        for (t = s; *t; ) {           /* while not at end of string */
        if (rep->next >= rep->end)  /* if the output buffer is full, */
        isr_flush(rep);           /* flush it (write it to the file) */
        *rep->next++ = *t++;        /* store the next string character */
        }
        return (int)(t-s);            /* return the number of characters */
        }  /* isr_puts() */
        
        /*--------------------------------------------------------------------*/
        
        static void isr_putsn (ISREPORT *rep, const char *s, int n)
        {                               /* --- write a character string */
        int k;                        /* number of chars in buffer */
        
        assert(rep);                  /* check the function arguments */
        while (n > 0) {               /* while there are characters left */
        k = (int)(rep->end -rep->next); /* get free space in write buffer */
        if (k >= n) {               /* if the string fits into buffer */
        memcpy(rep->next, s, (size_t)n *sizeof(char));
          rep->next += n; break;    /* simply copy the string into */
        }                           /* the write buffer and abort */
        memcpy(rep->next, s, (size_t)k *sizeof(char));
        s += k; n -= k; rep->next = rep->end;
        isr_flush(rep);             /* fill the buffer, then flush it, */
        }                             /* and reduce the remaining string */
        }  /* isr_putsn() */
        
        /*--------------------------------------------------------------------*/
        
        int isr_intout (ISREPORT *rep, ptrdiff_t num)
        {                               /* --- print an integer number */
        int  i = BS_INT, n;           /* loop variable, character counter */
        char buf[BS_INT];             /* output buffer */
        
        assert(rep);                  /* check the function arguments */
        if (rep->ints                 /* if pre-formatted ints. available */
        && (num >= (ptrdiff_t)rep->imin)    /* and the number is in range */
        && (num <= (ptrdiff_t)rep->imax))
        return isr_puts(rep, rep->ints[num -rep->imin]);
        if (num == 0) {               /* treat zero as a special case */
        isr_putc(rep, '0'); return 1; }
        if (num <= PTRDIFF_MIN) {     /* treat minimum as a special case */
#if PTRDIFF_MIN < 0x80000000/* if 64 bit system */
        isr_putsn(rep, "-9223372036854775808", 20); return 20;
#else                       /* if 32 bit system */
        isr_putsn(rep, "-2147483648", 11); return 11;
#endif                      /* directly return string and size */
        }
        n = 0;                        /* default: no sign printed */
        if (num < 0) {                /* if the number is negative, */
        num = -num; isr_putc(rep, '-'); n = 1; }  /* print a sign */
        do {                          /* digit output loop */
        buf[--i] = (char)((num % 10) +'0');  /* store the next digit */
        num /= 10;                  /* and remove it from the number */
        } while (num > 0);            /* while there are more digits */
        isr_putsn(rep, buf+i, BS_INT-i);
        n += BS_INT-i;                /* print the generated digits and */
        return n;                     /* return the number of characters */
        }  /* isr_intout() */
        
        /*--------------------------------------------------------------------*/
        
        int mantout (ISREPORT *rep, double num, int digits, int ints)
        {                               /* --- format a non-negative mantissa */
        int    i, n;                  /* loop variables, sign flag */
        double x, y;                  /* integral and fractional part */
        char   *s, *e, *d;            /* pointers into the output buffer */
        char   buf[BS_FLOAT];         /* output buffer */
        
        assert(rep);                  /* check the function arguments */
        i = (int)dbl_bisect(num, pows, 36);
        if ((i >= 36) || (pows[i] == num)) i++;
        n = digits -(i-2);            /* compute the number of decimals */
        x = floor(num); y = num-x;    /* split into integer and fraction */
        e = d = buf +40;              /* get buffer for the decimals */
        if (n > 0) {                  /* if to print decimal digits, */
        *e++ = '.';                 /* store a decimal point */
        do { y *= 10;               /* compute the next decimal */
        *e++ = (char)((int)y+'0');/* and store it in the buffer */
        y   -= floor(y);          /* remove the printed decimal */
        } while (--n > 0);          /* while there are more decimals */
        }                             /* remove a decimal if necessary */
        if ((y > 0.5) || ((y == 0.5)  /* if number needs to be rounded */
        &&  ((e > d) ? *(e-1) & 1 : floor(x/2) >= x/2))) {
          for (s = e; --s > d; ) {    /* traverse the decimal digits */
        if (*s < '9') { (*s)++; break; }
        *s = '0';                 /* if digit can be incremented, */
          }                           /* abort, otherwise store a zero */
        if ((s <= d) && ((x += 1) >= pows[i]))
          if (--e <= d+1) e = d;    /* if all decimals have been adapted, */
        }                             /* increment the integer part and */
        if (e > d) {                  /* if there are decimal places, */
        while (*--e == '0');        /* remove all trailing zeros */
        if (e > d) e++;             /* if there are no decimals left, */
        }                             /* also remove the decimal point */
        s = d;                        /* adapt the decimals if necessary */
        do {                          /* integral part output loop */
        *--s = (char)(fmod(x, 10) +'0');
          x = floor(x/10);            /* compute and store next digit */
        } while (x > 0);              /* while there are more digits */
        if ((n = (int)(d-s)) > ints)  /* check size of integral part */
        return -n;                  /* and abort if it is too large */
        isr_putsn(rep, s, n = (int)(e-s)); /* print the formatted number */
        return n;                     /* return the number of characters */
        }  /* mantout() */
        
        /*--------------------------------------------------------------------*/
        
        int isr_numout (ISREPORT *rep, double num, int digits)
        {                               /* --- print a floating point number */
        int  k, n, e;                 /* character counters and exponent */
        char buf[BS_FLOAT];           /* output buffer */
        
        assert(rep);                  /* check the function arguments */
        if (isnan(num)) {             /* check for 'not a number' */
        isr_putsn(rep, "nan", 3); return 3; }
        n = 0;                        /* default: no character printed */
        if (num < 0) {                /* if the number is negative, */
        num = -num; isr_putc(rep, '-'); n = 1; }  /* print a sign */
        if (isinf(num)) {             /* check for an infinite value */
        isr_putsn(rep, "inf", 3); return n+3; }
        if (num < DBL_MIN) {          /* check for a zero value */
        isr_putc(rep, '0');   return n+1; }
        if (digits > 32) digits = 32; /* limit the number of sign. digits */
        if (digits > 11) {            /* if very high precision is needed */
        k = sprintf(buf, "%.*g", digits, num);
          isr_putsn(rep, buf, k);     /* format with standard printf, */
        return n+k;                 /* print the formatted number and */
        }                             /* return the number of characters */
        e = 0;                        /* default: no exponential represent. */
        if ((num >= pows[digits+2])   /* if an exponential representation */
        ||  (num <  0.001)) {         /* is of the number is preferable */
        while (num <  1e00) { num *= 1e32; e -= 32; }
        while (num >= 1e32) { num /= 1e32; e += 32; }
        k = (int)dbl_bisect(num, pows+2, 34);
          if ((k >= 34) || (pows[k+2] != num)) k--;
          e += k;                     /* find and extract decimal exponent */
        num /= pows[k+2];           /* compute the new mantissa */
        }                             /* (one digit before decimal point) */
        k = mantout(rep, num, digits, (e == 0) ? digits : 1);
        if (k < 0) {                  /* try to output the mantissa */
        num /= pows[1-k]; e += -1-k;/* on failure adapt the mantissa */
        k = mantout(rep, num, digits, 1);
        }                             /* output the adapted number */
        n += k;                       /* compute number of printed chars. */
        if (e == 0) return n;         /* if no exponent, abort the function */
        isr_putc(rep, 'e'); n += 2;   /* print an exponent indicator */
        isr_putc(rep, (e < 0) ? '-' : '+');
        if ((e = abs(e)) < 10) { isr_putc(rep, '0'); n++; }
        k = BS_INT;                   /* get the end of the buffer */
        do {                          /* exponent digit output loop */
        buf[--k] = (char)((e % 10) +'0');    /* store the next digit */
        e /= 10;                    /* and remove it from the number */
        } while (e > 0);              /* while there are more digits */
        isr_putsn(rep, buf+k, BS_INT-k);
        return n+BS_INT-k;            /* print the generated digits and */
        }  /* isr_numout() */           /* return the number of characters */
        
        /*--------------------------------------------------------------------*/
        
        
#define isr_tidfinish(r)  isr_tidflush(r)
        static void isr_tidflush (ISREPORT *rep)
        {                               /* --- flush the output buffer */
        assert(rep);                  /* check the function arguments */
        fwrite(rep->tidbuf, sizeof(char), (size_t)(rep->tidnxt-rep->tidbuf),
               rep->tidfile);         /* write the output buffer */
        rep->tidnxt = rep->tidbuf;    /* reinit. next character pointer */
        /* in debug mode */
        fflush(rep->tidfile);         /* flush the output buffer */
        /* after every flush operation */
        }  /* isr_tidflush() */
        
        /*--------------------------------------------------------------------*/
        
        static void isr_tidputc (ISREPORT *rep, int c)
        {                               /* --- write a single character */
        assert(rep);                  /* check the function arguments */
        if (rep->tidnxt >= rep->tidend)     /* if output buffer is full, */
        isr_tidflush(rep);          /* flush it (write it to the file) */
        *rep->tidnxt++ = (char)c;     /* store the given character */
        }  /* isr_tidputc() */
        
        /*--------------------------------------------------------------------*/
        
        static void isr_tidputs (ISREPORT *rep, const char *s)
        {                               /* --- write a character string */
        assert(rep);                  /* check the function arguments */
        while (*s) {                  /* while not at end of string */
        if (rep->tidnxt >= rep->tidend)   /* if output buffer is full, */
        isr_tidflush(rep);        /* flush it (write it to the file) */
        *rep->tidnxt++ = *s++;      /* store the next string character */
        }
        }  /* isr_tidputs() */
        
        /*--------------------------------------------------------------------*/
        
        static void isr_tidputsn (ISREPORT *rep, const char *s, int n)
        {                               /* --- write a character string */
        int k;                        /* number of chars in buffer */
        
        assert(rep);                  /* check the function arguments */
        while (n > 0) {               /* while there are characters left */
        k = (int)(rep->tidend -rep->tidnxt); /* get free space in buffer */
        if (k >= n) {               /* if the string fits into buffer */
        memcpy(rep->tidnxt, s, (size_t)n *sizeof(char));
          rep->tidnxt += n; break;  /* simply copy the string into */
        }                           /* the write buffer and abort */
        memcpy(rep->tidnxt, s, (size_t)k *sizeof(char));
        s += k; n -= k; rep->tidnxt = rep->tidend;
        isr_tidflush(rep);          /* fill the buffer, then flush it, */
        }                             /* and reduce the remaining string */
        }  /* isr_tidputsn() */
        
        /*--------------------------------------------------------------------*/
        
        static void isr_tidout (ISREPORT *rep, TID tid)
        {                               /* --- print a transaction id */
        int  i;                       /* loop variable */
        char buf[BS_INT];             /* output buffer */
        
        assert(rep && (tid >= 0));    /* check the function arguments */
        if (rep->ints                 /* if pre-formatted ints. available */
        && (tid >= rep->imin)         /* and transaction id is in range */
        && (tid <= rep->imax)) {
        isr_tidputs(rep, rep->ints[tid -rep->imin]); return; }
        i = BS_INT;                   /* get end of output buffer */
        do {                          /* digit output loop (back to front) */
        buf[--i] = (char)((tid % 10) +'0');  /* store the next digit */
        tid /= 10;                  /* and remove it from the number */
        } while (tid > 0);            /* while there are more digits */
        isr_tidputsn(rep, buf+i, BS_INT-i);      /* print the digits */
        }  /* isr_tidout() */
        
        /*--------------------------------------------------------------------*/
        
        static void isr_occout (ISREPORT *rep, ITEM occ)
        {                               /* --- print an occurrence count */
        int  i;                       /* loop variable */
        char buf[BS_INT];             /* output buffer */
        
        assert(rep && (occ >= 0));    /* check the function arguments */
        if (rep->ints                 /* if pre-formatted ints. available */
        && ((TID)occ <= rep->imin)    /* and occurrence counter is in range */
        && ((TID)occ <= rep->imax)) {
        isr_tidputs(rep, rep->ints[occ -rep->imin]); return; }
        i = BS_INT;                   /* get end of output buffer */
        do {                          /* digit output loop (back to front) */
        buf[--i] = (char)((occ % 10) +'0');  /* store the next digit */
        occ /= 10;                  /* and remove it from the number */
        } while (occ > 0);            /* while there are more digits */
        isr_tidputsn(rep, buf+i, BS_INT-i);      /* print the digits */
        }  /* isr_occout() */
        
        /*----------------------------------------------------------------------
        Generator Filtering Functions
        ----------------------------------------------------------------------*/
        static size_t is_hash (const void *set, int type)
        {                               /* --- compute item set hash value */
        size_t     i;                 /* loop variable */
        size_t     h;                 /* computed hash value */
        const ITEM *p;                /* to access the items */
        
        assert(set);                  /* check the function argument */
        p = (const ITEM*)set;         /* type the item set pointer */
        h = (size_t)*p++;             /* get the number of items */
        i = (h >> 3) +1;              /* use Duff's device */
        switch (h & 7) {              /* to compute the hash value */
        do {    h = h *251 +(size_t)*p++;
        case 7: h = h *251 +(size_t)*p++;
        case 6: h = h *251 +(size_t)*p++;
        case 5: h = h *251 +(size_t)*p++;
        case 4: h = h *251 +(size_t)*p++;
        case 3: h = h *251 +(size_t)*p++;
        case 2: h = h *251 +(size_t)*p++;
        case 1: h = h *251 +(size_t)*p++;
        case 0: ; } while (--i > 0);
        }                             /* semicolon is necessary */
        return h;                     /* return the computed hash value */
        /* This hash function treats an item set like a string, that is, */
        /* the hash code depends on the order of the items. This is no   */
        /* drawback, though, because the comparison also requires that   */
        /* the items are in the same order in the item sets to compare.  */
        /* However, an order-independent hash code could be used to make */
        /* the function is_isgen() faster by avoiding recomputations.    */
        }  /* is_hash() */
        
        /*--------------------------------------------------------------------*/
        
        static int is_cmp (const void *a, const void *b, void *d)
        {                               /* --- compare two item sets */
        ITEM n;                       /* loop variable, number of items */
        ITEM *x, *y;                  /* to access the item sets */
        
        assert(a && b);               /* check the function arguments */
        x = (ITEM*)a; y = (ITEM*)b;   /* get/type the item set pointers */
        n = *x++;                     /* if the item set sizes differ, */
        if (n != *y++) return 1;      /* the item sets are not equal */
        while (--n >= 0)              /* traverse and compare the items */
        if (x[n] != y[n]) return 1; /* if an item differs, abort */
        return 0;                     /* otherwise the item sets are equal */
        /* Using memcmp() for the comparison is slower, because memcmp() */
        /* also checks the order relationship, not just equality, which, */
        /* however, is all that is needed inside the hash table.         */
        }  /* is_cmp() */
        
        /*--------------------------------------------------------------------*/
        
        static int is_isgen (ISREPORT *rep, ITEM item, RSUPP supp)
        {                               /* --- check for a generator */
        ITEM   i;                     /* loop variable */
        size_t z;                     /* key size */
        ITEM   *p;                    /* to access the hash table key */
        RSUPP  *s;                    /* to access the hash table data */
        ITEM   a, b;                  /* buffers for items (hold-out) */
        
        assert(rep && (item >= 0));   /* check the function arguments */
        rep->iset[rep->cnt+1] = item; /* store the new item at the end */
        if (rep->cnt > 0) {           /* if the current set is not empty */
        rep->iset[0] = rep->cnt;    /* copy the item set to the buffer */
        p = (ITEM*)memcpy(rep->iset+1, rep->items,
             (size_t)rep->cnt *sizeof(ITEM));
        if (rep->mode & ISR_SORT)   /* sort the items according to code */
        ia_qsort(p, (size_t)rep->cnt+1, rep->dir);
        a = p[i = rep->cnt];        /* note the first hold-out item */
        for (++i; --i >= 0; ) {     /* traverse the items in the set */
        b = p[i]; p[i] = a; a = b;/* get next subset (next hold-out) */
        if (a == item) continue;  /* do not exclude the new item */
        s = (RSUPP*)st_lookup(rep->gentab, rep->iset, 0);
        if (!s || (*s == supp))   /* if a subset with one item less */
        break;                  /* is not in the generator repository */
        }                           /* or has the same support, abort */
        if (i >= 0) return 0;       /* if subset was found, no generator */
        memmove(p+1, p, (size_t)rep->cnt *sizeof(ITEM));
        p[0] = a;                   /* restore the full new item set */
        }                             /* (with the proper item order) */
        rep->iset[0] = rep->cnt+1;    /* store the new item set size */
        z = (size_t)(rep->cnt+2) *sizeof(ITEM);  /* compute key size */
        s = (RSUPP*)st_insert(rep->gentab, rep->iset, 0, z, sizeof(RSUPP));
        if (!s) return -1;            /* add the new set to the repository */
        *s = supp;                    /* and store its support as the data */
        return 1;                     /* return 'set is a generator' */
        }  /* is_isgen() */
        
        int isr_delete (ISREPORT *rep, int delis)
        {                               /* --- delete an item set reporter */
        int r, s;                     /* results of file close operations */
        
        assert(rep);                  /* check the function arguments */
        if (rep->out) free(rep->out); /* delete the item set output buffer */
        if (rep->clomax) cm_delete(rep->clomax);
        if (rep->gentab) st_delete(rep->gentab);
        if (rep->psp)    psp_delete(rep->psp);
        if (rep->str)    free((void*)rep->str);
        if (rep->border) free(rep->border);
#ifndef ISR_NONAMES
{ ITEM i;                     /* loop variable */
        for (i = 0; rep->inames[i]; i++) {
          if (rep->inames[i] != ib_name(rep->base, i))
            free((void*)rep->inames[i]);
        }                           /* delete existing item names */
}                             /* delete all other arrays */
#endif
        if (rep->ints)   free(rep->ints);
        if (rep->stats)  free(rep->stats);
        if (rep->wgts)   free(rep->wgts);
        if (rep->supps)  free(rep->supps);
        if (rep->iset)   free(rep->iset);
        if (rep->pxpp)   free(rep->pxpp);
        if (rep->base && delis) ib_delete(rep->base);
        r = isr_close(rep);           /* close the output files */
        s = isr_tidclose(rep);        /* (if output files are open) */
        if (rep->tidbuf) free(rep->tidbuf);
        if (rep->buf)    free(rep->buf); /* delete file write buffers */
        free(rep);                    /* delete the base structure */
        return (r) ? r : s;           /* return file closing result */
        }  /* isr_delete() */
        
        /*--------------------------------------------------------------------*/
        
        int isr_settarg (ISREPORT *rep, int target, int mode, int dir)
        {                               /* --- set target and operation mode */
        assert(rep);                  /* check the function arguments */
        if      (target & ISR_RULES)   target = ISR_RULES;
        else if (target & ISR_GENERAS) target = ISR_GENERAS;
        else if (target & ISR_MAXIMAL) target = ISR_MAXIMAL;
        else if (target & ISR_CLOSED)  target = ISR_CLOSED;
        else                           target = ISR_ALL;
        if (target & (ISR_CLOSED|ISR_MAXIMAL))
          mode |= ISR_NOEXPAND;       /* make reporting mode consistent */
        rep->target = target;         /* note target (closed/maximal etc) */
        rep->mode   = mode;           /* and reporting mode */
        if (rep->clomax) { cm_delete(rep->clomax); rep->clomax = NULL; }
        if (rep->gentab) { st_delete(rep->gentab); rep->gentab = NULL; }
        if ((target & (ISR_MAXIMAL|ISR_CLOSED|ISR_GENERAS))
              &&  !(mode & ISR_NOFILTER)) { /* if to filter the item sets */
        if (target & ISR_GENERAS) { /* if to filter for generators, */
        size_t n = 1024*1024-1;   /* create an item set hash table */
        rep->gentab = st_create(n, 0, is_hash, is_cmp, NULL, (OBJFN*)0);
        if (!rep->gentab) return E_NOMEM; }
        else {                      /* if to filter for closed/maximal */
        rep->clomax = cm_create(dir, ib_cnt(rep->base));
          if (!rep->clomax) return E_NOMEM;
        }                           /* create a closed/maximal filter */
        rep->sto = (target & ISR_MAXIMAL) ? 0 : RSUPP_MAX;
        rep->dir = (dir < 0) ? -1 : 1;
        }                             /* note storage flag and direction */
        fastchk(rep);                 /* check for fast output */
        return 0;                     /* return 'ok '*/
        }  /* isr_settarg() */
        
        /*--------------------------------------------------------------------*/
        
        void isr_setsupp (ISREPORT *rep, RSUPP smin, RSUPP smax)
        {                               /* --- set support range for item set */
        assert(rep                    /* check the function arguments */
        &&    (smin >= 0) && (smax >= smin));
          rep->smin = smin;             /* store the minimum and maximum */
        rep->smax = smax;             /* support of an item set to report */
        }  /* isr_setsupp() */
        
        /*--------------------------------------------------------------------*/
        
        void isr_setsize (ISREPORT *rep, ITEM zmin, ITEM zmax)
        {                               /* --- set size range for item set */
        assert(rep                    /* check the function arguments */
        &&    (zmin >= 0) && (zmax >= zmin));
          rep->zmin = zmin;             /* store the minimum and maximum */
        rep->zmax = zmax;             /* size of an item set to report */
        fastchk(rep);                 /* check for fast output */
        }  /* isr_setsize() */
        
        void isr_seteval (ISREPORT *rep, ISEVALFN evalfn, void *data,
                          int dir, double thresh)
        {                               /* --- set evaluation function */
        assert(rep);                  /* check the function argument */
        rep->evalfn  = evalfn;        /* store the evaluation function, */
        rep->evaldat = data;          /* the corresponding user data, */
        rep->evaldir = (dir >= 0) ? +1 : -1;  /* the evaluation direction */
        rep->evalthh = rep->evaldir *thresh;  /* and the threshold value  */
        fastchk(rep);                 /* check for fast output */
        }  /* isr_seteval() */
        
        /*--------------------------------------------------------------------*/
        
        void isr_setrepo (ISREPORT *rep, ISREPOFN repofn, void *data)
        {                               /* --- set set reporting function */
        assert(rep);                  /* check the function argument */
        rep->repofn  = repofn;        /* store the reporting function and */
        rep->repodat = data;          /* the corresponding user data */
        fastchk(rep);                 /* check for fast output */
        }  /* isr_setrepo() */
        
        /*--------------------------------------------------------------------*/
        
        void isr_setrule (ISREPORT *rep, ISRULEFN rulefn, void *data)
        {                               /* --- set rule reporting function */
        assert(rep);                  /* check the function argument */
        rep->rulefn  = rulefn;        /* store the reporting function and */
        rep->ruledat = data;          /* the corresponding user data */
        }  /* isr_setrule() */
        
        /*--------------------------------------------------------------------*/
        
        int isr_prefmt (ISREPORT *rep, TID min, TID max)
        {                               /* --- pre-format transaction ids */
        TID  t, z;                    /* to traverse the integers to format */
        char *s, *b, *c, *e;          /* next position, buffer positions */
        char buf[BS_INT+1];           /* to pre-format an integer number */
        
        if (rep->ints) {              /* clear pre-formatted integers */
        free(rep->ints); rep->ints = NULL; }
        if (max < 0) return 0;        /* if no preformatting, abort */
        if (min < 0) min = 0;         /* ensure a non-negative minimum */
        rep->imin = min;              /* note the range of integers */
        rep->imax = max;              /* that will be pre-formatted */
        max += 1; z = max+max;        /* one digit plus end-of string */
        for (t = 10; (max >= t) && (TID_MAX /10 > t); t *= 10)
          z += max-t;                 /* compute preformatted size */
        z -= min+min;                 /* one digit plus end-of string */
        for (t = 10; (min >= t) && (TID_MAX /10 > t); t *= 10)
          z -= min-t;                 /* compute preformatted size */
        t = max-min;                  /* get the number of integers */
        rep->ints = (char**)malloc((size_t)t*sizeof(char*)
                                     +(size_t)z*sizeof(char));
                                     if (!rep->ints) return -1;    /* get buffer for preformatted nums. */
        s = (char*)(rep->ints +t);    /* and position of first integer */
        memset(buf, '0', sizeof(buf));/* fill preformat buffer with zeros */
        e = b = buf+BS_INT; *b = 0;   /* and get pointer to last digit */
        t = min;                      /* format the minimum */
        do {                          /* digit output loop */
        *--b = (char)((t % 10)+'0');/* store the next digit */
        t /= 10;                    /* and remove it from the number */
        } while (t > 0);              /* while there are more digits */
        for (t = min; t < max; t++) { /* traverse the transaction ids */
        rep->ints[t-min] = memcpy(s, b, (size_t)(e-b)+1);
          s += (e-b)+1;               /* copy the number representation */
        for (c = e; --c >= buf; ) { /* advance the number representation */
        if (*c >= '9') *c = '0'; else { (*c)++; break; } }
        if (c < b) b = c;           /* adapt the start of the number */
        }                             /* and advance the pointer */
        return 0;                     /* return 'ok' */
        }  /* isr_prefmt() */
        
        /*--------------------------------------------------------------------*/
        
        int isr_close (ISREPORT *rep)
        {                               /* --- close the output file */
        int r;                        /* result of fclose()/fflush() */
        
        assert(rep);                  /* check the function arguments */
        if (!rep->file) return 0;     /* check for an output file */
        isr_finish(rep);              /* flush the write buffer */
        r  = ferror(rep->file);       /* check the error indicator */
        r |= ((rep->file == stdout) || (rep->file == stderr))
          ? fflush(rep->file) : fclose(rep->file);
        rep->file = NULL;             /* close the current output file */
        fastchk(rep);                 /* check for fast output */
        return r;                     /* return the result of fclose() */
        }  /* isr_close() */
        
        /*--------------------------------------------------------------------*/
        
        int isr_tidclose (ISREPORT *rep)
        {                               /* --- close trans. id output file */
        int r;                        /* result of fclose() */
        
        assert(rep);                  /* check the function arguments */
        if (!rep->tidfile) return 0;  /* check for an output file */
        isr_tidfinish(rep);           /* flush the write buffer */
        r  = ferror(rep->tidfile);    /* check the error indicator */
        r |= ((rep->tidfile == stdout) || (rep->tidfile == stderr))
          ? fflush(rep->tidfile) : fclose(rep->tidfile);
        rep->tidfile = NULL;          /* close the current output file */
        fastchk(rep);                 /* check for fast output */
        return r;                     /* return the result of fclose() */
        }  /* isr_tidclose() */
        
        int isr_setup (ISREPORT *rep)
        {                               /* --- set up the item set reporter */
        size_t h, s, z;               /* lengths, size of output buffer */
        
        assert(rep);                  /* check the function arguments */
        if (rep->out) free(rep->out); /* delete an existing output buffer */
        h = strlen(rep->hdr);         /* compute the output buffer size */
        s = strlen(rep->sep);         /* (items + header + separators) */
        z = (rep->mode & ISR_SEQUENCE)
          ? (size_t)rep->size *rep->nmax : rep->nsum;
        z += h +(size_t)(rep->size-1) *s +1;
        rep->out = (char*)malloc(z *sizeof(char));
        if (!rep->out) return E_NOMEM;/* create an output buffer and */
        strcpy(rep->out, rep->hdr);   /* copy the record header into it */
        rep->pos[0] = rep->out +h;    /* store the first item position */
        rep->pfx    = rep->cnt = 0;   /* initialize the item counters */
        rep->xmax   = ((rep->target & (ISR_CLOSED|ISR_MAXIMAL))
                         && (rep->zmax < ITEM_MAX)) ? rep->zmax+1 : rep->zmax;
        fastchk(rep);                 /* check for fast output */
        return 0;                     /* return 'ok '*/
        }  /* isr_setupx() */
        
        /*--------------------------------------------------------------------*/
        
        int isr_add (ISREPORT *rep, ITEM item, RSUPP supp)
        {                               /* --- add an item (only support) */
        assert(rep && (item >= 0)     /* check the function arguments */
        &&    (item < ib_cnt(rep->base)));
          /* assert(!isr_uses(rep, item)); */
          /* if (supp < rep->smin) return 0; */
          if      (rep->clomax) {       /* if a closed/maximal filter exists */
          int r = cm_add(rep->clomax, item, supp);
            if (r <= 0) return r; }     /* add the item to the c/m filter */
          else if (rep->gentab) {       /* if a generator filter exists */
          int r = is_isgen(rep, item, supp);
            if (r <= 0) return r;       /* add item set to the gen. filter */
          }                             /* check if item needs processing */
          rep->pxpp [item] |= ITEM_MIN; /* mark the item as used */
          rep->items[  rep->cnt] = item;/* store the item and its support */
          rep->supps[++rep->cnt] = supp;/* clear the perfect ext. counter */
          rep->pxpp [  rep->cnt] &= ITEM_MIN;
          return 1;                     /* return 'ok' */
        }  /* isr_add() */
          
          int isr_addpex (ISREPORT *rep, ITEM item)
          {                               /* --- add a perfect extension */
          assert(rep && (item >= 0)     /* check the function arguments */
          &&    (item < ib_cnt(rep->base)));
            if ((rep->pxpp[item] < 0)     /* if the item is already in use */
          ||  (rep->target & ISR_GENERAS))/* or to filter for generators, */
          return -1;                  /* perfect extensions are ignored */
          rep->pxpp[item] |= ITEM_MIN;  /* mark the item as used */
          *--rep->pexs = item;          /* store the added item and */
          rep->pxpp[rep->cnt]++;        /* count it for the current prefix */
          return 0;                     /* return 'ok' */
          }  /* isr_addpex() */
          
          /*--------------------------------------------------------------------*/
          
          void isr_addpexpk (ISREPORT *rep, ITEM bits)
          {                               /* --- add a perfect extension */
          ITEM i;                       /* loop variable/item */
          
          assert(rep);                  /* check the function arguments */
          bits &= ~ITEM_MIN;            /* traverse the set bits */
          for (i = 0; (UITEM)(1 << i) <= (UITEM)bits; i++) {
            if (((bits & (1 << i)) == 0)/* if the bit is not set */
          || (rep->pxpp[i] < 0)       /* or the item is already in use */
          || (rep->target & ISR_GENERAS)) /* or to filter for generators, */
          continue;                 /* perfect extensions are ignored */
          rep->pxpp[i] |= ITEM_MIN;   /* mark the item as used */
          *--rep->pexs = i;           /* store the added item and */
          rep->pxpp[rep->cnt] += 1;   /* count it for the current prefix */
          }
          }  /* isr_addpexpk() */
          
          /*--------------------------------------------------------------------*/
          
          void isr_remove (ISREPORT *rep, ITEM n)
          {                               /* --- remove one or more items */
          ITEM i;                       /* loop variable, buffer for an item */
          
          assert(rep                    /* check the function arguments */
          &&    (n >= 0) && (n <= rep->cnt));
          if (rep->clomax)              /* if a closed/maximal filter exists, */
          cm_remove(rep->clomax, n);  /* remove the same number of items */
          while (--n >= 0) {            /* traverse the items to remove */
          for (i = rep->pxpp[rep->cnt] & ~ITEM_MIN; --i >= 0; )
            rep->pxpp[*rep->pexs++] &= ~ITEM_MIN;
            i = rep->items[--rep->cnt]; /* traverse the item to remove */
          rep->pxpp[i] &= ~ITEM_MIN;  /* (current item and perfect exts.) */
          }                             /* and remove their "in use" markers */
          if (rep->cnt < rep->pfx)      /* if too few items are left, */
          rep->pfx = rep->cnt;        /* reduce the valid prefix */
          }  /* isr_remove() */
          
          /*--------------------------------------------------------------------*/
          
          double isr_logrto (ISREPORT *rep, void *data)
          {                               /* --- logarithm of support ratio */
          ITEM   i;                     /* loop variable */
          double sum;                   /* sum of item logarithms */
          
          assert(rep);                  /* check the function arguments */
          if (rep->cnt <= 1) return 0;  /* if only one item, abort */
          sum = (double)rep->supps[rep->cnt];
          sum = (sum > 0) ? log(sum /(double)rep->supps[0]) /LN_2 : 0;
          for (i = 0; i < rep->cnt; i++)
            sum -= rep->ldps[rep->items[i]];
          return sum;                   /* compute log of support ratio */
          }  /* isr_logrto() */
          
          /* Evaluate an itemset by the logarithm of the quotient of the actual */
          /* support of an item set and the support that is expected under full */
          /* independence of the items (product of item probabilities times the */
          /* total transaction weight). 'data' is needed for the interface.     */
          static void fastout (ISREPORT *rep, ITEM n)
          {                               /* --- fast output of an item set */
          char       *s;                /* to traverse the output buffer */
          const char *name;             /* to traverse the item names */
          
          assert(rep);                  /* check the function argument */
          rep->stats[rep->cnt] += 1;    /* count the reported item set */
          rep->repcnt          += 1;    /* (for its size and overall) */
          if (rep->psp)                 /* count item set in pattern spectrum */
          psp_incfrq(rep->psp, rep->cnt, rep->supps[rep->cnt], 1);
          s = rep->pos[rep->pfx];       /* get the position for appending */
          while (rep->pfx < rep->cnt) { /* traverse the additional items */
          if (rep->pfx > 0)           /* if this is not the first item */
          for (name = rep->sep; *name; )
            *s++ = *name++;         /* copy the item separator */
          for (name = rep->inames[rep->items[rep->pfx]]; *name; )
            *s++ = *name++;           /* copy the item name to the buffer */
          rep->pos[++rep->pfx] = s;   /* compute and record new position */
          }                             /* for appending the next item */
          while (n > 0) {               /* traverse the perfect extensions */
          rep->items[rep->cnt++] = rep->pexs[--n];
            fastout(rep, n);            /* add the next perfect extension, */
          rep->pfx = --rep->cnt;      /* recursively report supersets, */
          }                             /* and remove the item again */
          isr_putsn(rep, rep->out, (int)(s-rep->out)); /* print item set */
          isr_putsn(rep, rep->foinfo, rep->fosize);    /* and its support */
          }  /* fastout() */
          
          /*--------------------------------------------------------------------*/
          
          static void output (ISREPORT *rep)
          {                               /* --- output an item set */
          TID        k;                 /* loop variable */
          ITEM       min;               /* minimum number of items */
          char       *s;                /* to traverse the output buffer */
          const char *name;             /* to traverse the item names */
          
          assert(rep                    /* check the function arguments */
          &&    (rep->cnt >= rep->zmin)
                   &&    (rep->cnt <= rep->zmax));
                   if (rep->border               /* if there is a filtering border */
          && (rep->cnt < rep->bdrcnt)   /* and the set size is in its range */
          && (rep->supps[rep->cnt] < rep->border[rep->cnt]))
          return;                     /* check the item set signature */
          if (rep->evalfn) {            /* if an evaluation function is given */
          rep->eval = rep->evalfn(rep, rep->evaldat);
            if (rep->evaldir *rep->eval < rep->evalthh)
              return;                   /* if the item set does not qualify, */
          }                             /* abort the output function */
          rep->stats[rep->cnt] += 1;    /* count the reported item set */
          rep->repcnt          += 1;    /* (for its size and overall) */
          if (rep->psp)                 /* count item set in pattern spectrum */
          psp_incfrq(rep->psp, rep->cnt, rep->supps[rep->cnt], 1);
          if (rep->repofn)              /* call reporting function if given */
          rep->repofn(rep, rep->repodat);
          if (!rep->file) return;       /* check for an output file */
          s = rep->pos[rep->pfx];       /* get the position for appending */
          while (rep->pfx < rep->cnt) { /* traverse the additional items */
          if (rep->pfx > 0)           /* if this is not the first item */
          for (name = rep->sep; *name; )
            *s++ = *name++;         /* copy the item separator */
          for (name = rep->inames[rep->items[rep->pfx]]; *name; )
            *s++ = *name++;           /* copy the item name to the buffer */
          rep->pos[++rep->pfx] = s;   /* compute and record new position */
          }                             /* for appending the next item */
          isr_putsn(rep, rep->out, (int)(s-rep->out));
          isr_sinfo(rep, rep->supps[rep->cnt], rep->wgts[rep->cnt], rep->eval);
          isr_putc (rep, '\n');         /* print the item set information */
          if (!rep->tidfile || !rep->tids) /* check whether to report */
          return;                        /* a list of transaction ids */
          if      (rep->tidcnt > 0) {   /* if tids are in ascending order */
          for (k = 0; k < rep->tidcnt; k++) {
            if (k > 0) isr_tidputs(rep, rep->sep);
            isr_tidout(rep, rep->tids[k]+1);
          } }                         /* report the transaction ids */
          else if (rep->tidcnt < 0) {   /* if tids are in descending order */
          for (k = -rep->tidcnt; k > 0; ) {
            isr_tidout(rep, rep->tids[--k]+1);
            if (k > 0) isr_tidputs(rep, rep->sep);
          } }                         /* report the transaction ids */
          else if (rep->tracnt > 0) {   /* if item occurrence counters */
          min = (ITEM)(rep->cnt-rep->miscnt); /* traverse all trans. ids */
          for (k = 0; k < rep->tracnt; k++) {
            if (rep->occs[k] < min)   /* skip all transactions that */
          continue;               /* do not contain enough items */
          if (k > 0) isr_tidputs(rep, rep->sep);
          isr_tidout(rep, k+1);     /* print the transaction identifier */
          if (rep->miscnt <= 0) continue;
          isr_tidputc(rep, ':');    /* print an item counter separator */
          isr_occout(rep, rep->occs[k]);
          }                           /* print number of contained items */
          }
          isr_tidputc(rep, '\n');       /* terminate the transaction id list */
          }  /* output() */
          
          /*--------------------------------------------------------------------*/
          
          static void report (ISREPORT *rep, ITEM n)
          {                               /* --- recursively report item sets */
          assert(rep && (n >= 0));      /* check the function arguments */
          while (n > 0) {               /* traverse the perfect extensions */
          rep->items[rep->cnt++] = rep->pexs[--n];
            if ((rep->cnt+n >= rep->zmin)  /* if a valid size can be reached */
          &&  (rep->cnt   <= rep->zmax)) /* (in the interval [min, max]), */
          report(rep, n);              /* recursively report supersets */
          if (--rep->cnt < rep->pfx)  /* remove the current item again */
          rep->pfx = rep->cnt;      /* and adapt the valid prefix */
          }
          if (rep->cnt >= rep->zmin)    /* if item set has minimum size, */
          output(rep);                /* report the current item set */
          }  /* report() */
          
          /*--------------------------------------------------------------------*/
          
          int isr_report (ISREPORT *rep)
          {                               /* --- report the current item set */
          ITEM   n, k;                  /* number of perfect extensions */
          ITEM   z;                     /* item set size */
          size_t m, c;                  /* buffers for item set counting */
          double w;                     /* buffer for an item set weight */
          RSUPP  s;                     /* support buffer */
          RSUPP  r;                     /* support buffer */
          ITEM   *items;                /* item set for prefix tree update */
          
          assert(rep);                  /* check the function argument */
          n = isr_pexcnt(rep);          /* get the number of perfect exts. */
          s = rep->supps[rep->cnt];     /* and the support of the item set */
          if (rep->clomax) {            /* if a closed/maximal filter exists */
          r = cm_supp(rep->clomax);   /* get the maximal known support */
          if (r >= s)        return 0;/* check if item set is not closed */
          if (r >= rep->sto) return 0;/* check whether to store item set */
          k = rep->cnt +n;            /* compute the total number of items */
          if (n <= 0)                 /* if there are no perfect extensions */
          items = rep->items;       /* the items can be used directly */
          else {                      /* if there are perfect extensions */
          items = (ITEM*)memcpy(rep->iset,rep->pexs,(size_t)k*sizeof(ITEM));
            ia_qsort(items, (size_t)k, rep->dir);
          }                           /* copy and sort the items in the set */
          if (cm_update(rep->clomax, items, k, s) < 0)
            return -1;                /* add the item set to the filter */
          if ((rep->target & ISR_MAXIMAL) && (r >= 0))
            return  0;                /* check for a non-maximal item set */
          }                             /* (if the known support is > 0) */
          if ((s < rep->smin) || (s > rep->smax))
            return 0;                   /* check the item set support */
          if ((rep->cnt   > rep->zmax)  /* if the item set is too large or */
          ||  (rep->cnt+n < rep->zmin)) /* the minimum size cannot be reached */
          return 0;                   /* with prefect extensions, abort */
          if (rep->fast < 0) {          /* if just to count the item sets */
          /* if no output is produced and no item sets can be filtered out, */
          /* compute the number of item sets in the perfect ext. hypercube. */
          s = rep->supps[rep->cnt];   /* get the support of the item set */
          if (rep->mode & ISR_NOEXPAND) {
            z = rep->cnt +n;          /* if not to expand perfect exts., */
          rep->stats[z] += 1;       /* count only one item set */
          rep->repcnt   += 1;       /* (for its size and overall) */
          if (rep->psp && (psp_incfrq(rep->psp, z, s, 1) < 0))
            return -1;              /* if a pattern spectrum exists, */
          return 0;                 /* return 'ok' */
          }
          m = 0; z = rep->cnt;        /* and init. the item set counter */
          if (z >= rep->zmin) {       /* if the item set is large enough */
          rep->stats[z] += 1;       /* count the current item set */
          m += 1;                   /* (for its size and overall) */
          if (rep->psp && (psp_incfrq(rep->psp, z, s, 1) < 0))
            return -1;              /* if a pattern spectrum exists, */
          }
          for (c = 1, k = 1; (k <= n) && (++z <= rep->zmax); k++) {
            c = (c *(size_t)(n-k+1))  /* compute n choose k */
          / (size_t)k;            /* for 1 <= k <= n */
          if (z >= rep->zmin) {     /* count the current item set */
          rep->stats[z] += c; m += c; /* (for its size and overall) */
          if (rep->psp && (psp_incfrq(rep->psp, z, s, c) < 0))
            return -1;            /* if a pattern spectrum exists, */
          }
          }                           /* (n choose k is the number of */
          rep->repcnt += m;           /* item sets of size rep->cnt +k) */
          return 0;                   /* return 'ok' */
          }
          /* It is debatable whether this way of handling perfect extensions  */
          /* in case no output is produced is acceptable for fair benchmarks, */
          /* because the sets in the hypercube are not explicitly generated.  */
          if (rep->fast)                /* format support for fast output */
          rep->fosize = sprintf(rep->foinfo, " (%"RSUPP_FMT")\n",
            rep->supps[rep->cnt]);
          if (rep->mode & ISR_NOEXPAND){/* if not to expand perfect exts. */
          k = rep->cnt +n;            /* if all perfext extensions make */
          if (k > rep->zmax) return 0;/* the item set too large, abort */
          rep->supps[k] = rep->supps[rep->cnt];
          rep->wgts [k] = rep->wgts [rep->cnt];
          for (k = n; --k >= 0; )     /* add all perfect extensions */
          rep->items[rep->cnt++] = rep->pexs[k];
          if (rep->fast) fastout(rep, 0); /* report the expanded set */
          else           output (rep);    /* (fast or normal output) */
          rep->cnt -= n;              /* remove the perfect extensions */
          /* in debug mode */
          isr_flush(rep);             /* flush the output buffer */
          /* after every item set */
          return 0;                   /* abort the function */
          }                             /* (all reporting has been done) */
          if (rep->fast)                /* if fast output is possible, */
          fastout(rep, n);            /* report item sets recursively */
          else {                        /* if fast output is not possible */
          s = rep->supps[rep->cnt];   /* set support and weights */
          w = rep->wgts [rep->cnt];   /* for perfect extension hypercube */
          for (k = 0; ++k <= n; ) { rep->supps[rep->cnt+k] = s;
            rep->wgts [rep->cnt+k] = w; }
          report(rep, n);             /* recursively add perfect exts. and */
          }                             /* report the resulting item sets */
          if (rep->psp && psp_error(rep->psp))
            return -1;                  /* check whether updating the */
          /* in debug mode */
          isr_flush(rep);               /* flush the output buffer */
          /* after every item set */
          return 0;                     /* return 'ok' */
          }  /* isr_report() */
          /*--------------------------------------------------------------------*/
          int isr_addpsp (ISREPORT *rep, PATSPEC *psp)
          {                               /* --- add a pattern spectrum */
          assert(rep);                  /* check the function arguments */
          if (rep->psp) return 1;       /* if pattern spectrum exists, abort */
          if (!psp) {                   /* if to create a pattern spectrum */
          psp = psp_create(rep->zmin, rep->zmax, rep->smin, rep->smax);
            if (!psp) return -1;        /* create a pattern spectrum */
          }                             /* with the stored limits */
          rep->psp = psp;               /* note the pattern spectrum */
          return 0;                     /* return 'ok' */
          }  /* isr_addpsp() */
          
          /*--------------------------------------------------------------------*/
          
          int isr_sinfo (ISREPORT *rep, RSUPP supp, double wgt, double eval)
          {                               /* --- print item set information */
          int        k, n = 0;          /* number of decimals, char. counter */
          double     sdbl, smax, wmax;  /* (maximum) support and weight */
          const char *s, *t;            /* to traverse the format */
          
          assert(rep);                  /* check the function arguments */
          if (!rep->info || !rep->file)
            return 0;                   /* check for a given format and file */
          sdbl = (double)supp;          /* get support as double prec. number */
          smax = (double)rep->supps[0]; /* get maximum support and */
          if (smax <= 0) smax = 1;      /* avoid divisions by zero */
          wmax =         rep->wgts[0];  /* get maximum weight  and */
          if (wmax <= 0) wmax = 1;      /* avoid divisions by zero */
          for (s = rep->info; *s; ) {   /* traverse the output format */
          if (*s != '%') {            /* copy everything except '%' */
          isr_putc(rep, *s++); n += 1; continue; }
          t = s++; k = getsd(s, &s);  /* get the number of signif. digits */
          switch (*s++) {             /* evaluate the indicator character */
          case '%': isr_putc(rep, '%'); n += 1;               break;
          case 'i': n += isr_intout(rep, (ptrdiff_t)rep->cnt);  break;
#define int    1
#define double 2
          case 'a': n += isr_intout(rep, (ptrdiff_t)supp);    break;
          case 'q': n += isr_intout(rep, (ptrdiff_t)smax);    break;
          case 'Q': n += isr_intout(rep, (ptrdiff_t)smax);    break;
#undef int
#undef double
          case 's': n += isr_numout(rep,      sdbl/smax,  k); break;
          case 'S': n += isr_numout(rep, 100*(sdbl/smax), k); break;
          case 'x': n += isr_numout(rep,      sdbl/smax,  k); break;
          case 'X': n += isr_numout(rep, 100*(sdbl/smax), k); break;
          case 'w': n += isr_numout(rep,      wgt,        k); break;
          case 'W': n += isr_numout(rep, 100* wgt,        k); break;
          case 'r': n += isr_numout(rep,      wgt /wmax,  k); break;
          case 'R': n += isr_numout(rep, 100*(wgt /wmax), k); break;
          case 'z': n += isr_numout(rep,      wgt *smax,  k); break;
          case 'e': n += isr_numout(rep,      eval,       k); break;
          case 'E': n += isr_numout(rep, 100* eval,       k); break;
          case 'p': n += isr_numout(rep,      eval,       k); break;
          case 'P': n += isr_numout(rep, 100* eval,       k); break;
          case  0 : --s;            /* print the requested quantity */
          default : isr_putsn(rep, t, k = (int)(s-t)); n += k; t = s; break;
          }                           /* otherwise copy characters */
          }
          return n;                     /* return the number of characters */
          }  /* isr_sinfo() */
          
          /*----------------------------------------------------------------------
        Preprocessor Definitions
        ----------------------------------------------------------------------*/
          /* --- operation modes --- */
#define IST_PERFECT 0x0100      /* prune with perfect extensions */
#define IST_PARTIAL 0x0200      /* do only partial subset checks */
#define IST_REVERSE 0x0400      /* reverse item order */
          
          /* --- additional evaluation measures --- */
          /* evaluation measure definitions in ruleval.h */
#define IST_INVBXS  INT_MIN     /* invalidate eval. below exp. supp. */
          
          /* --- evaluation measure aggregation modes --- */
#define IST_NONE    0           /* no aggregation (use first value) */
#define IST_FIRST   0           /* no aggregation (use first value) */
#define IST_MIN     1           /* minimum of measure values */
#define IST_MAX     2           /* maximum of measure values */
#define IST_AVG     3           /* average of measure values */
          
          /* --- item set filter modes --- */
#define IST_CLEAR   ISR_ALL     /* all     frequent item sets */
#define IST_CLOSED  ISR_CLOSED  /* closed  frequent item sets */
#define IST_MAXIMAL ISR_MAXIMAL /* maximal frequent item sets */
#define IST_GENERA  ISR_GENERA  /* generators */
#define IST_SAFE    ISR_SORT    /* safe filtering (assume holes) */
          
          /*----------------------------------------------------------------------
          Type Definitions
----------------------------------------------------------------------*/
          typedef struct istnode {        /* --- item set tree node --- */
          struct istnode *succ;         /* successor node (on same level) */
          struct istnode *parent;       /* parent    node (one level up) */
          ITEM           item;          /* item used in parent node */
          ITEM           offset;        /* offset of counter array */
          ITEM           size;          /* size   of counter array */
          ITEM           chcnt;         /* number of child nodes */
          SUPP           cnts[1];       /* counter array (weights) */
          } ISTNODE;                      /* (item set tree node) */
          
          typedef struct {                /* --- item set tree --- */
          ITEMBASE *base;               /* underlying item base */
          int      mode;                /* search mode (e.g. support def.) */
          SUPP     wgt;                 /* total weight of transactions */
          ITEM     height;              /* tree height (number of levels) */
          ISTNODE  **lvls;              /* first node of each level */
          int      valid;               /* whether levels are valid */
          SUPP     smin;                /* minimum support of an item set */
          SUPP     body;                /* minimum support of a rule body */
          double   conf;                /* minimum confidence of a rule */
          int      eval;                /* additional evaluation measure */
          int      agg;                 /* aggregation mode of measure values */
          int      invbxs;              /* invalidate eval. below expectation */
          double   dir;                 /* direction of evaluation measure */
          double   thresh;              /* evaluation measure threshold */
          ISTNODE  *curr;               /* current node for traversal */
          ITEM     depth;               /* depth of current node */
          ITEM     size;                /* current size of an item set */
          ITEM     zmin;                /* minimal size of an item set */
          ITEM     zmax;                /* maximal size of an item set */
          int      order;               /* item set output order (by size) */
          ISTNODE  *node;               /* item set node for extraction */
          ITEM     index;               /* index in item set node */
          ISTNODE  *head;               /* head item node for extraction */
          ITEM     prune;               /* start level for evaluation pruning */
          ITEM     item;                /* head item of previous rule */
          ITEM     *buf;                /* buffer for paths (support check) */
          ITEM     *path;               /* current path / (partial) item set */
          int      hdonly;              /* head only item in current set */
          ITEM     *map;                /* to create identifier maps */
          } ISTREE;                       /* (item set tree) */
          
          /*----------------------------------------------------------------------
          Functions
          ----------------------------------------------------------------------*/
          void      ist_delete  (ISTREE *ist);
          
          void      ist_up      (ISTREE *ist);
          int       ist_down    (ISTREE *ist, ITEM item);
          SUPP      ist_setsupp (ISTREE *ist, ITEM item, SUPP supp);
          int       ist_xable   (ISTREE *ist, ITEM n);
          int       ist_addchn  (ISTREE *ist);
          /*----------------------------------------------------------------------
          Preprocessor Definitions
          ----------------------------------------------------------------------*/
#define ist_xable(t,n)    ((t)->depth+(n) <= (t)->zmax)
          
          /*----------------------------------------------------------------------
          Preprocessor Definitions
----------------------------------------------------------------------*/
#define LN_2        0.69314718055994530942  /* ln(2) */
#define HDONLY      ITEM_MIN    /* flag for head only item in path */
#define ITEMOF(n)   ((ITEM)((n)->item & ~HDONLY))
#define ISHDONLY(n) ((n)->item < 0)
#define int         1           /* to check definition of SUPP */
#define long        2           /* for double precision type */
#define double      3
#define SKIP        SUPP_MIN    /* flag for subtree skipping */
#define SETSKIP(n)  ((n) |=  SKIP)
#define CLRSKIP(n)  ((n) &= ~SKIP)
#define IS2SKIP(n)  ((n) < 0)
#define COUNT(n)    ((n) &  ~SKIP)
#define INC(n,w)    ((n) += (w))
#undef int                      /* remove preprocessor definitions */
#undef long                     /* needed for the type checking */
#undef double
#define CHILDCNT(n) ((n)->chcnt & ~ITEM_MIN)
#define ITEMAT(n,i) (((n)->offset >= 0) ? (n)->offset +(i) \
          : ((ITEM*)((n)->cnts +(n)->size))[i])
        
        /*----------------------------------------------------------------------
            Functions
            ----------------------------------------------------------------------*/
        
        double chi2pdf (double x, double df)
        {                               /* --- probability density function */
        assert(df > 0);               /* check the function arguments */
        if (x <=  0) return 0;        /* only non-zero for positive arg. */
        if (df == 2) return 0.5 *exp(-0.5*x);
        df *= 0.5;                    /* compute probability density */
        return 0.5 *exp((df-1) *log(0.5*x) -0.5*x -logGamma(df));
        }  /* chi2pdf() */
        
        double chi2cdfQ (double x, double df)
        {                               /* --- cumulative distribution fn. */
        assert(df > 0);               /* check the function arguments */
        return GammaQ(0.5*df, 0.5*x); /* compute regularized Gamma function */
        }  /* chi2cdfQ() */
        /*----------------------------------------------------------------------
          Auxiliary Functions
          ----------------------------------------------------------------------*/
        
        static ITEM search (ITEM id, ISTNODE **chn, ITEM n)
        {                               /* --- find a child node (index) */
        ITEM l, r, m;                 /* left, right, and middle index */
        ITEM x;                       /* item of middle child */
        
        assert(chn && (n > 0));       /* check the function arguments */
        for (l = 0, r = n; l < r; ) { /* while the range is not empty */
        m = (l+r) >> 1;             /* get index of the middle element */
        x = ITEMOF(chn[m]);         /* compare the item identifier */
        if      (id > x) l = m+1;   /* to the middle element and */
        else if (id < x) r = m;     /* adapt the range boundaries */
        else return m;              /* if there is an exact match, */
        }                             /* return the child node index */
        return (ITEM)-1;              /* return 'not found' */
        }  /* search() */
        
        /*--------------------------------------------------------------------*/
        /*--------------------------------------------------------------------*/
        
        static SUPP getsupp3 (ISTNODE *node, ITEM *items, ITEM n)
        {                               /* --- get support of an item set */
        ITEM    i, k;                 /* array indices, number of children */
        ISTNODE **chn;                /* child node array */
        
        assert(node                   /* check the function arguments */
        &&    (n >= 0) && (items || (n <= 0)));
        for ( ; --n > 0; items++) {   /* follow the set/path from the node */
        k = CHILDCNT(node);         /* if there are no children, */
        if (k <= 0) return SKIP;    /* the support is less than minsupp */
        if (node->offset >= 0) {    /* if a pure array is used */
        chn = (ISTNODE**)(node->cnts +node->size);
          ALIGN(chn);               /* get the child array index */
        i = *items -ITEMOF(chn[0]);  /* compute the child array index */
        if (i >= k) return SKIP; }   /* and check whether entry exists */
        else {                      /* if an identifier map is used */
        chn = (ISTNODE**)((ITEM*)(node->cnts +node->size) +node->size);
          ALIGN(chn);               /* get the child array index */
        i = search(*items, chn, k);
        }                           /* find the child array index */
        if (i < 0) return SKIP;     /* if child does not exist, abort */
        node = chn[i];              /* go to the corresponding child */
        if (!node) return SKIP;     /* if child does not exist, abort */
        }                             /* (support is less than minsupp) */
        k = node->size;               /* get the number of counters */
        if (node->offset >= 0) {      /* if a pure array is used, */
        i = *items -node->offset;   /* compute the counter index and */
        if (i >= k) return SKIP; }  /* check whether counter exists */
        else                          /* if an identifier map is used */
        i = ia_bsearch(*items, (ITEM*)(node->cnts +k), (size_t)k);
        if (i < 0) return SKIP;       /* if no counter exists, abort */
        return node->cnts[i];         /* return the item set support */
        }  /* getsupp3() */
        
        static void delete (ISTNODE *node)
        {                               /* --- recursively delete a subtree */
        ITEM    i, n;                 /* loop variable, number of children */
        ISTNODE **chn;                /* child node array */
        
        assert(node);                 /* check the function argument */
        n = CHILDCNT(node);           /* get the number of children */
        if (n > 0) {                  /* if there are children */
        if (node->offset >= 0)      /* if a pure array is used */
        chn = (ISTNODE**)(node->cnts +node->size);
        else                        /* if an item map is used */
        chn = (ISTNODE**)((ITEM*)(node->cnts +node->size) +node->size);
        ALIGN(chn);                 /* get the child node array */
        for (i = 0; i < n; i++)     /* recursively delete the children */
        if (chn[i]) delete(chn[i]);
        }
        free(node);                   /* delete the node */
        }  /* delete() */
        
        /*----------------------------------------------------------------------
          Main Functions
          ----------------------------------------------------------------------*/
        
        
        void ist_delete (ISTREE *ist)
        {                               /* --- delete an item set tree */
        ITEM    h;                    /* loop variable */
        ISTNODE *node, *t;            /* to traverse the nodes */
        
        assert(ist);                  /* check the function argument */
        if (!ist->valid)              /* if levels are not valid */
        delete(ist->lvls[0]);       /* use recursive deletion */
        else {                        /* if levels are valid */
        for (h = ist->height; --h >= 0; ) {
          for (node = ist->lvls[h]; node; ) {
            t = node; node = node->succ; free(t); }
        }                           /* delete all nodes */
        }                             /* by traversing the levels */
        free(ist->lvls);              /* delete the level array, */
        free(ist->map);               /* the identifier map, */
        free(ist->buf);               /* the path buffer, */
        free(ist);                    /* and the tree body */
        }  /* ist_delete() */
        
        
        static ISTNODE* child (ISTREE *ist, ISTNODE *node, ITEM index, SUPP pex)
        {                               /* --- create child node (extend set) */
        ITEM    i, k, n, m, e;        /* loop variables, counters */
        ISTNODE *curr;                /* to traverse the path to the root */
        ITEM    item;                 /* item identifier */
        ITEM    *set;                 /* next (partial) item set to check */
        int     body;                 /* enough support for a rule body */
        int     hdonly;               /* whether head only item on path */
        int     app;                  /* appearance flags of an item */
        SUPP    supp;                 /* support of an item set */
        
        assert(ist && node            /* check the function arguments */
        &&    (index >= 0) && (index < node->size));
        
        /* --- initialize --- */
        supp = node->cnts[index];     /* get support of item set to extend */
        if ((supp <  ist->smin)       /* if the support is insufficient */
        ||  (supp >= pex))            /* or item is a perfect extension, */
        return NULL;                /* abort (do not create a child) */
        item = ITEMAT(node, index);   /* get the item for the index and */
        app  = ib_getapp(ist->base, item);    /* the corresp. app. flag */
        if ((app == APP_NONE)         /* do not extend an item to ignore */
        || ((app == APP_HEAD) && (ISHDONLY(node))))
          return NULL;                /* do not combine two head only items */
        hdonly = (app == APP_HEAD) || ISHDONLY(node);
        body   = (supp >= ist->body)  /* if the set has enough support for */
        ? 1 : 0;               /* a rule body, set the body flag */
        ist->buf[ist->height-1] = item;/* init. set for support checks */
        
        /* --- check candidates --- */
        if (ist->mode & IST_REVERSE) { i = -1;    e = index;      }
        else                         { i = index; e = node->size; }
        for (m = 0; ++i < e; ) {      /* traverse the relevant indices */
        k   = ITEMAT(node, i);      /* retrieve the associated item */
        app = ib_getapp(ist->base, k);
        if ((app == APP_NONE) || (hdonly && (app == APP_HEAD)))
          continue;                 /* skip sets with two head only items */
        supp = node->cnts[i];       /* traverse the candidate items */
        if ((supp <  ist->smin)     /* if set support is insufficient */
        ||  (supp >= pex))          /* or item is a perfect extension, */
        continue;                 /* ignore the corresponding candidate */
        body &= 1;                  /* restrict body flags to set support */
        if (supp >= ist->body)      /* if set support is sufficient for */
        body |= 2;                /* a rule body, set the body flag */
        curr = node;                /* start at the current node */
        if (ist->mode & IST_PARTIAL)/* if to check only some subsets */
        body |= 4;                /* assume sufficient body support */
        else {                      /* if to check all subsets */
        set    = ist->buf +ist->height+1 -(n = 2);
          set[1] = k;               /* add the candidate item to the set */
        for ( ; curr->parent; curr = curr->parent) {
          supp = getsupp3(curr->parent, set, n);
          if (supp <  ist->smin)  /* get the subset support and */
        break;                /* if it is too low, abort loop */
        if (supp >= ist->body)  /* if some subset has enough support */
        body |= 4;            /* for a rule body, set the body flag */
        *--set = ITEMOF(curr);  /* add id of current node to the set */
        n += 1;                 /* and adapt the number of items */
        }
        }
        if (!curr->parent && body)  /* if subset support is high enough */
        ist->map[m++] = k;        /* for a full rule and a rule body, */
        }                             /* note the item identifier */
        if (m <= 0) return NULL;      /* if no child is needed, abort */
        
        /* --- decide on node structure --- */
        n = ist->map[m-1] -ist->map[0] +1;
        k = (m+m < n) ? n = m : 0;    /* compute the range of items */
        
        /* --- create child --- */
        curr = (ISTNODE*)malloc(sizeof(ISTNODE) +(size_t)(n-1) *sizeof(SUPP)
                                  +(size_t) k    *sizeof(ITEM));
        if (!curr) return (ISTNODE*)-1;      /* create a child node */
        if (hdonly) item |= HDONLY;   /* set the head only flag and */
        curr->item  = item;           /* initialize the item identifier */
        curr->chcnt = 0;              /* there are no children yet */
        curr->size  = n;              /* set size of counter array */
        if (k <= 0) {                 /* if to use a pure array, note */
        curr->offset = k = ist->map[0];  /* first item as an offset */
        for (i = 0; i < n; i++) curr->cnts[i] = SKIP;
        for (i = 0; i < m; i++) curr->cnts[ist->map[i]-k] = 0; }
        else {                        /* if to use an identifier map, */
        curr->offset = -1;          /* use negative offset as indicator */
        memset(curr->cnts,    0,        (size_t)n *sizeof(SUPP));
        memcpy(curr->cnts +n, ist->map, (size_t)n *sizeof(ITEM));
        }                             /* clear counters, copy item id. map */
        return curr;                  /* return pointer to created child */
        }  /* child() */
        
        static ISTNODE** children (ISTREE *ist, ISTNODE **np, ISTNODE **end)
        {                               /* --- create children of a node */
        ITEM    i, n;                 /* loop variable, node counter */
        size_t  z;                    /* size of counter and map arrays */
        SUPP    pex;                  /* support for a perfect extension */
        ISTNODE *node;                /* node to get children */
        ISTNODE *par;                 /* parent of current node */
        ISTNODE *cur;                 /* current node in new level (child) */
        ISTNODE **frst;               /* first child of current node */
        ISTNODE *last;                /* last  child of current node */
        ISTNODE **chn;                /* child node array */
        
        assert(ist && np && end);     /* check the function arguments */
        node = *np;                   /* get the node to get children */
        frst = end; last = NULL;      /* note start of the child node list */
        if (!(ist->mode & IST_PERFECT)) pex = SUPP_MAX;
        else if (!node->parent)         pex = ist->wgt;
        else pex = getsupp3(node->parent, &node->item, 1);
        pex = COUNT(pex);             /* get support for perfect extension */
        for (i = n = 0; i < node->size; i++) {
          cur = child(ist,node,i,pex);/* traverse the counter array */
        if (!cur) continue;         /* create a child node if necessary */
        if (cur == (void*)-1) { *end = NULL; return NULL; }
        *end = last = cur;          /* add node at the end of the list */
        end  = &cur->succ; n++;     /* that contains the new level */
        }                             /* and advance the end pointer */
        *end = NULL;                  /* terminate the child node list */
        if (n <= 0) {                 /* if no child node was created, */
        node->chcnt = ITEM_MIN; return end; }       /* skip the node */
        chn = np; par = node->parent; /* get the parent node */
        if (par) {                    /* if there is a parent node */
        if (par->offset >= 0) {     /* if a pure array is used */
        chn = (ISTNODE**)(par->cnts +par->size);
          ALIGN(chn);               /* get the child node array */
        chn += ITEMOF(node) -ITEMOF(chn[0]); }
        else {                      /* if an identifier map is used */
        chn = (ISTNODE**)((ITEM*)(par->cnts +par->size) +par->size);
          ALIGN(chn);               /* get the child node array */
        chn += search(ITEMOF(node), chn, CHILDCNT(par));
        }                           /* find the child node pointer */
        }                             /* in the parent node */
        /* The location of the child pointer in the parent node must be  */
        /* retrieved here, because ITEMOF(chn[0]) (for a pure array) may */
        /* access already freed memory after the node has been resized,  */
        /* since it may be that node = chn[0] and then chn[0] may become */
        /* by the reallocation of the node to add the child pointers.    */
        if (node->offset >= 0) {      /* if a pure counter array is used */
        z = (size_t)(node->size-1) *sizeof(SUPP);
          n = ITEMOF(last) -ITEMOF(*frst) +1; } /* pure child array */
        else {                        /* if an identifier map is used */
        z = (size_t)(node->size-1) *sizeof(SUPP)
          + (size_t) node->size    *sizeof(ITEM);
        }                             /* add a compact child array */
        z += sizeof(ISTNODE);         /* add the node size */
        node = (ISTNODE*)realloc(node, z+PAD(z) +(size_t)n*sizeof(ISTNODE*));
        if (!node) return NULL;       /* add a child array to the node */
        *np = *chn = node;            /* update the node pointer and */
        node->chcnt = n;              /* note the number of children */
        if (node->offset >= 0) {      /* if a pure array is used */
        chn = (ISTNODE**)(node->cnts +node->size);
          ALIGN(chn);                 /* get the child node array */
        while (--n >= 0) chn[n] = NULL;
        i = ITEMOF(*frst);          /* get the child node array */
        for (cur = *frst; cur; cur = cur->succ) {
          chn[ITEMOF(cur)-i] = cur; /* set the child node pointer */
        cur->parent = node;       /* and the parent pointer */
        } }
        else {                        /* if an identifier map is used */
        chn = (ISTNODE**)((ITEM*)(node->cnts +node->size) +node->size);
          ALIGN(chn);                 /* get the child node array */
        for (i = 0, cur = *frst; cur; cur = cur->succ) {
          chn[i++]    = cur;        /* set the child node pointer */
        cur->parent = node;       /* and the parent pointer */
        }                           /* in the new node */
        }                             /* (store pointers to children) */
        return end;                   /* return new end of node list */
        }  /* children() */
        
        /*--------------------------------------------------------------------*/
        
        void ist_up (ISTREE *ist)
        {                               /* --- go up in item set tree */
        assert(ist && ist->curr);     /* check the function argument */
        if (!ist->curr->parent) return;  /* check for a parent node */
        ist->curr   = ist->curr->parent; /* go to the parent node and */
        ist->depth -= 1;                 /* decrease the node depth */
        }  /* ist_up() */
        
        /*--------------------------------------------------------------------*/
        
        int ist_down (ISTREE *ist, ITEM item)
        {                               /* --- go down in item set tree */
        ISTNODE *node;                /* current node */
        ISTNODE **chn;                /* child node array */
        ITEM    cnt, i;               /* number of children, index */
        
        assert(ist && ist->curr);     /* check the function argument */
        node = ist->curr;             /* get the current node */
        cnt  = CHILDCNT(node);        /* if there are no child nodes, */
        if (cnt <= 0) return -1;      /* abort the function */
        if (node->offset >= 0) {      /* if a pure array is used */
        chn = (ISTNODE**)(node->cnts +node->size);
          ALIGN(chn);                 /* get the child node array */
        i   = item -ITEMOF(chn[0]); /* compute index in child node array */
        if ((i < 0) || (i >= cnt) || !chn[i]) return -1; }
        else {                        /* if an identifier map is used */
        chn = (ISTNODE**)((ITEM*)(node->cnts +node->size) +node->size);
          ALIGN(chn);                 /* get the child node array */
        i   = search(item, chn, cnt);
        if (i < 0) return -1;       /* search for the item in the map */
        }                             /* and check whether child exists */
        ist->curr   = chn[i];         /* go to the child node and */
        ist->depth += 1;              /* increase the node depth */
        return 0;                     /* return 'ok' */
        }  /* ist_down() */
        
        SUPP ist_setsupp (ISTREE *ist, ITEM item, SUPP supp)
        {                               /* --- get support for an item */
        ITEM    i;                    /* array index */
        ITEM    *map;                 /* item identifier map */
        ISTNODE *node;                /* current node in tree */
        
        assert(ist && ist->curr);     /* check the function argument */
        node = ist->curr;             /* get the current node */
        if (node->offset >= 0) {      /* if pure arrays are used, */
        i = item -node->offset;     /* get index in counter array */
        if (i >= node->size) return 0; }
        else {                        /* if an identifier map is used */
        map = (ITEM*)(node->cnts +node->size);
          i   = ia_bsearch(item, map, (size_t)node->size);
        }                             /* search the item in the map */
        if (i < 0) return 0;          /* abort if item not found */
        node->cnts[i] = supp;         /* set new item set support */
        return COUNT(supp);           /* return the item set support */
        }  /* ist_setsupp() */
        
        int ist_addchn (ISTREE *ist)
        {                               /* --- add a child node */
        ISTNODE *list;                /* list of child nodes */
        ISTNODE **end = &list;        /* end of node list of new level */
        
        assert(ist && ist->curr);     /* check the function argument */
        if (CHILDCNT(ist->curr) > 0)  /* if there are children already, */
        return 1;                   /* abort the function */
        end = children(ist, &ist->curr, end);
        if (!end) return -1;          /* add children to the current node */
        if (ist->depth <= 1)          /* if currently at the root node, */
        ist->lvls[0] = ist->curr;   /* update the root node */
        if (ist->depth+1 > ist->height)
          ist->height = ist->depth+1; /* update the tree height */
        ist->valid = 0;               /* levels/successors are not valid */
        return 0;                     /* return that children were added */
        }  /* ist_addchn() */
        
        /*--------------------------------------------------------------------*/
        
        
#define _POSIX_C_SOURCE 200809L /* needed for sigaction */
        
        
        /*----------------------------------------------------------------------
        Functions
----------------------------------------------------------------------*/
        void sig_install (void);
        void sig_remove  (void);
        void sig_abort   (int state);
        int  sig_aborted (void);
        
        /*----------------------------------------------------------------------
        Global Variables
        ----------------------------------------------------------------------*/
        static volatile sig_atomic_t aborted = 0;
        /* whether abort interrupt received */
        static struct sigaction sigold; /* old signal action (to restore) */
        static struct sigaction signew; /* new signal action (this module) */
        
        /*----------------------------------------------------------------------
        External Functions
        ----------------------------------------------------------------------*/
        
        static void sighandler (int type)
        { if (type == SIGINT) sig_abort(-1); }
        /*--------------------------------------------------------------------*/
        void sig_install (void)
        {                               /* --- install signal handler */
        signew.sa_handler = sighandler;
          signew.sa_flags   = 0;
          sigemptyset(&signew.sa_mask);
          sigaction(SIGINT, &signew, &sigold);
        }  /* siginstall() */
        /*--------------------------------------------------------------------*/
        void sig_remove (void)          /* --- remove signal handler */
        { sigaction(SIGINT, &sigold, (struct sigaction*)0); }
        /*--------------------------------------------------------------------*/
        
        void sig_abort (int state)
        { aborted = state; }            /* --- set the abort state */
        /*--------------------------------------------------------------------*/
        int sig_aborted (void)
        {                               /* --- check the abort state */
        return aborted;
        }
        
        /*----------------------------------------------------------------------
        Type Definitions
        ----------------------------------------------------------------------*/
        typedef unsigned short BITTA;   /* --- bit rep. of a transaction --- */
        
        typedef struct {                /* --- FIM 16 items machine --- */
        ISREPORT *report;             /* item set reporter */
        int      dir;                 /* processing direction */
        SUPP     smin;                /* minimum support */
        SUPP     ttw;                 /* total transaction weight */
        BITTA    tor;                 /* bitwise or of added trans. */
        SUPP     *wgts;               /* transaction weights */
        ITEM     *map;                /* item identifier map */
        SUPP     supps[16];           /* support values of items */
        BITTA    *btas[16];           /* array of bit rep. transactions */
        BITTA    *ends[16];           /* ends of transaction arrays */
        } FIM16;                        /* (FIM 16 items machine) */
        
        /*----------------------------------------------------------------------
        Functions
        ----------------------------------------------------------------------*/
        FIM16* m16_create (int dir, SUPP supp, ISREPORT *report);
        void   m16_delete (FIM16 *fim);
        void   m16_setmap (FIM16 *fim, int i, ITEM id);
        void   m16_clrmap (FIM16 *fim);
        void   m16_add    (FIM16 *fim, BITTA tract, SUPP wgt);
        int    m16_mine   (FIM16 *fim);
        
        /*----------------------------------------------------------------------
        Preprocessor Definitions
        ----------------------------------------------------------------------*/
        
#define m16_setmap(m,i,d)   ((m)->map[i] = (d))
        
#define BLKSIZE         1024    /* block size for arrays */
#define IS_NA(x)        (isnan(x) || isinf(x) || ((x) < 0))
        
        /* --- error handling --- */
#define MYERROR(msg)    do { sig_remove(); error(msg); } while (0)
#define ERR_MEM()       MYERROR("out of memory")
#define ERR_ABORT()     MYERROR("user abort")
        
        /*----------------------------------------------------------------------
        Preprocessor Definitions
----------------------------------------------------------------------*/
#define E_STDIN      (-5)       /* double assignment of stdin */
#define E_OPTION     (-6)       /* unknown option */
#define E_OPTARG     (-7)       /* missing option argument */
#define E_ARGCNT     (-8)       /* too few/many arguments */
#define E_TARGET     (-9)       /* invalid target type */
#define E_SIZE      (-10)       /* invalid item set size */
#define E_SUPPORT   (-11)       /* invalid minimum item set support */
#define E_ITEMCNT   (-12)       /* invalid number of items */
#define E_MEASURE   (-13)       /* invalid evaluation measure */
#define E_REPEAT    (-14)       /* invalid number of repetitions */
        /* error codes -15 to -25 defined in tract.h */
#define CLOCK(t)    ((t) = clock())
        
#define SEC_SINCE(t)  ((double)(clock()-(t)) /(double)CLOCKS_PER_SEC)
        
        static unsigned char hibit[1 << 16] = { 1 };
        /* map from bit representation of a transaction to highest set bit */
        
        static const BITTA prjms[16] = {/* bit masks for trans. projection */
        0x0000, 0x0001, 0x0003, 0x0007,
        0x000f, 0x001f, 0x003f, 0x007f,
        0x00ff, 0x01ff, 0x03ff, 0x07ff,
        0x0fff, 0x1fff, 0x3fff, 0x7fff };
        
        /*----------------------------------------------------------------------
        16 Items Machine Functions
        ----------------------------------------------------------------------*/
        FIM16* m16_create (int dir, SUPP supp, ISREPORT *report)
        {                               /* --- create a 16 items machine */
        int   i, k, n, s;             /* loop variables, buffers */
        FIM16 *fim, *cur;             /* created 16 items machine */
        BITTA *p;                     /* to organize the memory */
        
        if (hibit[0] != 0)            /* init. highest bit map if necessary */
        for (k = i = 0; k < 16; k++)/* traverse all possible transactions */
        while (i < (1 << (k+1))) hibit[i++] = (unsigned char)k;
        n   = (dir > 0) ? 1 : 16;     /* get the number of sub-machines */
        fim = (FIM16*)calloc((size_t)n, sizeof(FIM16));
        if (!fim) return NULL;        /* allocate the base structure */
        fim->map = (ITEM*)malloc(16*sizeof(ITEM));
        if (!fim->map) { free(fim); return NULL; }
        m16_clrmap(fim);              /* create an item identifier map */
        for (cur = fim, s = 16; --n >= 0; cur++, s--) {
          cur->report = report;       /* traverse the 16 items sub-machines */
        cur->dir    = dir;          /* and initialize the basic fields */
        cur->smin   = supp;         /* (i.e., direction, minimum support */
        cur->ttw    = 0;            /* and the total transaction weight) */
        cur->wgts   = (SUPP*)calloc(((size_t)1) << s, sizeof(SUPP));
        if (!cur->wgts) { m16_delete(cur); return NULL; }
        k = (s > 10) ? 10 : s;      /* allocate the trans. weight array */
        p = (BITTA*)malloc((((size_t)1) << k) *sizeof(BITTA));
        if (!p) { m16_delete(fim); return NULL; }
        for (i = 0; i < k; i++) {   /* allocate one block for short lists */
        cur->ends[i] = cur->btas[i] = p; p += (i > 1) ? 1 << i : 2; }
        for ( ; i < s; i++) {       /* individual blocks for the rest */
        p = (BITTA*)malloc((((size_t)1) << i) *sizeof(BITTA));
          if (!p) { m16_delete(fim); return NULL; }
          cur->ends[i] = cur->btas[i] = p;
        } /* This allocation scheme looks certainly weird, but showed the */
        }   /* best performance in several experiments; reasons are unclear.*/
        /* The difference is bigger on a 32 bit system than on 64 bit.  */
        return fim;                   /* return created 16 items machine */
        }  /* m16_create() */
        
        /*--------------------------------------------------------------------*/
        
        void m16_delete (FIM16 *fim)
        {                               /* --- delete a 16 items machine */
        int   i, n, s;                /* loop variable, buffers */
        FIM16 *cur;                   /* to traverse the machines */
        
        assert(fim);                  /* check the function arguments */
        n = (fim->dir < 0) ? 16 : 1;  /* traverse the machines */
        for (cur = fim, s = 16; --n >= 0; cur++, s--) {
          for (i = s; --i >= 10; )    /* traverse the transaction arrays */
        if (cur->btas[i]) free(cur->btas[i]);
        if (cur->btas[0]) free(cur->btas[0]);
        free(cur->wgts);            /* delete the transaction arrays */
        }                             /* and the transaction weight array */
        free(fim->map);               /* delete the item identifier map */
        free(fim);                    /* delete the base structure */
        }  /* m16_delete() */
        
        /*--------------------------------------------------------------------*/
        
        void m16_clrmap (FIM16 *fim)
        {                               /* --- clear item identifier map */
        fim->map[ 0] =  0; fim->map[ 1] =  1; fim->map[ 2] =  2;
        fim->map[ 3] =  3; fim->map[ 4] =  4; fim->map[ 5] =  5;
        fim->map[ 6] =  6; fim->map[ 7] =  7; fim->map[ 8] =  8;
        fim->map[ 9] =  9; fim->map[10] = 10; fim->map[11] = 11;
        fim->map[12] = 12; fim->map[13] = 13; fim->map[14] = 14;
        fim->map[15] = 15;            /* set item map to identity */
        }  /* m16_clrmap() */
        
        /*--------------------------------------------------------------------*/
        
        void m16_add (FIM16 *fim, BITTA tract, SUPP wgt)
        {                               /* --- add a transaction in bit rep. */
        assert(fim);                  /* check the function arguments */
        fim->ttw += wgt;              /* sum the transaction weight */
        if (tract == 0) return;       /* ignore empty transactions */
        fim->tor |= tract;            /* combine for single trans. check */
        if ((fim->wgts[tract] += wgt) <= wgt)  /* update trans. weight and */
        *fim->ends[hibit[tract]]++ = tract;  /* add it to its hibit list */
        }  /* m16_add() */
        
        /*--------------------------------------------------------------------*/
        
        static void project (FIM16 *fim, ITEM item, BITTA mask, FIM16 *dst)
        {                               /* --- project trans. to an item */
        BITTA *t, *e;                 /* to traverse the transactions */
        BITTA p;                      /* projected trans. (item removed) */
        SUPP  *w;                     /* (location of) transaction weight */
        
        assert(fim && dst && (item > 0)); /* check the function arguments */
        dst->map = fim->map;          /* copy the item identifier map */
        mask &= prjms[item];          /* remove the item from the mask */
        fim->supps[item] = 0;         /* clear the item support (reinit.) */
        e = fim->ends[item];          /* get and clear the trans. range */
        fim->ends[item] = t = fim->btas[item];
        for ( ; t < e; t++) {         /* traverse the item's transactions */
        w = fim->wgts +*t;          /* note the trans. weight location */
        p = *t & mask;              /* project transaction (remove item) */
        if (p) {                    /* if the projection is not empty */
        if ((dst->wgts[p] += *w) <= *w)     /* add the projection */
        *dst->ends[hibit[p]]++ = p;       /* to the transaction list */
        }                           /* of its highest bit (if necessary) */
        *w = 0;                     /* update the projection support and */
        }                             /* clear support of original trans. */
        }  /* project() */
        
        /* A separate function without a destination (projecting a machine   */
        /* to itself, for rec_pos()) does not improve the processing speed.  */
        /* Note that the check whether p is empty can be avoided if one sets */
        /* dst->wgts[0] = 1, which prevents p = 0 being added for item 0.    */
        /* However, this rather seems to slow down processing somewhat.      */
        
        /*--------------------------------------------------------------------*/
        
        static void count2 (FIM16 *fim, int n)
        {                               /* --- count support of all items */
        BITTA *t, *e;                 /* to traverse the transactions */
        BITTA p;                      /* projected trans. (item removed) */
        SUPP  w, s;                   /* trans. weight and item support */
        
        assert(fim && (n > 0));       /* check the function arguments */
        while (--n > 1) {             /* traverse the (lower) items */
        e = fim->ends[n]; t = fim->btas[n];
        for (s = 0; t < e; t++) {   /* traverse the item's transactions */
        s += w = fim->wgts[*t];   /* note and sum the trans. weight */
        p = *t & prjms[n];        /* project transaction (remove item) */
        if (!p) continue;         /* if projection is empty, skip it */
        if ((fim->wgts[p] += w) <= w)  /* update projection support */
        *fim->ends[hibit[p]]++ = p;  /* and add the projection to */
        }                                /* the list of its highest bit */
        fim->supps[n] = s;          /* store the computed item support */
        }                             /* finally count for items 0 and 1 */
        fim->supps[1] = fim->wgts[2] +fim->wgts[3];
        fim->supps[0] = fim->wgts[1] +fim->wgts[3];
        }  /* count2() */
        
        /*--------------------------------------------------------------------*/
        
        static void clear2 (FIM16 *fim, int item)
        {                               /* --- clear a transaction list */
        BITTA *t, *e;                 /* to traverse the transactions */
        
        assert(fim && (item >= 0));   /* check the function arguments */
        fim->supps[item] = 0;         /* clear the item support (reinit.) */
        e = fim->ends[item];          /* get and clear the trans. range */
        fim->ends[item] = t = fim->btas[item];
        for ( ; t < e; t++) fim->wgts[*t] = 0;
        }  /* clear2() */                /* clear all transaction weights */
        
        /*--------------------------------------------------------------------*/
        
        static BITTA filter (FIM16 *fim, int n, SUPP pex)
        {                               /* --- filter items from projection */
        BITTA mask;                   /* item mask for needed items */
        
        assert(fim && (pex > 0));     /* check the function arguments */
        for (mask = 0; --n >= 0; ) {  /* traverse the items */
        if      (fim->supps[n] <  fim->smin)
          clear2(fim, n);            /* eliminate infrequent items */
        else if (fim->supps[n] >= pex) {
          clear2(fim, n);            /* collect perfect extensions */
        isr_addpex(fim->report, fim->map[n]); }
        else mask = (BITTA)(mask | (1 << n));
        }                             /* if item is frequent, set bit mask */
        return mask;                  /* return the item mask */
        }  /* filter() */
        
        /*--------------------------------------------------------------------*/
        
        static int rec_pos (FIM16 *fim, int n, BITTA mask)
        {                               /* --- find frequent item sets */
        int   i, r;                   /* loop variable, error status */
        SUPP  s;                      /* item support */
        BITTA m;                      /* filtering mask */
        
        assert(fim && (n >= 0));      /* check the function arguments */
        /* The item mask needs to be checked, because items 0,1,2 may have  */
        /* been identified as perfect extensions in higher recursion levels.*/
        if ((mask & 1)                /* if item 0 is frequent */
        &&  (fim->supps[0] >= fim->smin)) {
          r = isr_add(fim->report, fim->map[0], fim->supps[0]);
          if (r < 0) return -1;       /* add item 0 to the reporter */
        if (r > 0) {                /* if item 0 needs processing */
        if (isr_report(fim->report) < 0)
          return -1;              /* report the current item set */
        isr_remove(fim->report,1);/* remove the item 0 again */
        }                           /* from the item set reporter */
        }
        if ((mask & 2)                /* if item 1 is frequent */
        &&  (fim->supps[1] >= fim->smin)) {
          r = isr_add(fim->report, fim->map[1], fim->supps[1]);
          if (r < 0) return -1;       /* add item 1 to the reporter */
        if (r > 0) {                /* if item 1 needs processing, */
        if ((mask & 1)            /* if item 0 is frequent given 1 */
        &&  (fim->wgts[3] >= fim->smin)) {
          if (fim->wgts[3] >= fim->supps[1])
            isr_addpex(fim->report, fim->map[0]);
          else {                  /* check for a perfect extension */
        r = isr_add(fim->report, fim->map[0], fim->wgts[3]);
            if (r < 0) return -1; /* add item 0 to the reporter */
        if (r > 0) {          /* if item 0 needs processing */
        if (isr_report(fim->report) < 0)
          return -1;        /* report the current item set */
        isr_remove(fim->report, 1);
        }                     /* remove the item 0 again */
          }                       /* from the item set reporter */
        }
        if (isr_report(fim->report) < 0)
          return -1;              /* report the current item set */
        isr_remove(fim->report,1);/* remove the item 1 again */
        }                           /* from the item set reporter */
        }
        if (n <= 2) {                 /* if only two items to process */
        memset(fim->wgts+1, 0, 3*sizeof(SUPP));
          memset(fim->supps,  0, 2*sizeof(SUPP));
          memcpy(fim->ends, fim->btas, 2*sizeof(BITTA*));
          return 0;                   /* clear counters for items 0 and 1 */
        }                             /* and clear the transaction lists */
        if ((mask & 4)                /* if item 2 is frequent */
        &&  (fim->supps[2] >= fim->smin)) {
          r = isr_add(fim->report, fim->map[2], fim->supps[2]);
          if (r < 0) return -1;       /* add item 2 to the reporter */
        if (r > 0) {                /* if  item 2 needs processing, */
        m = 0;                    /* project/count for items 0 and 1 */
        fim->wgts[3]  = fim->wgts[7];
        fim->wgts[2]  = fim->wgts[6];
        fim->wgts[1]  = fim->wgts[5];
        fim->supps[0] = s = fim->wgts[3] +fim->wgts[1];
        if      (s >= fim->supps[2]) isr_addpex(fim->report, fim->map[0]);
        else if (s >= fim->smin)     m = (BITTA)(m | (mask & 1));
        fim->supps[1] = s = fim->wgts[3] +fim->wgts[2];
        if      (s >= fim->supps[2]) isr_addpex(fim->report, fim->map[1]);
        else if (s >= fim->smin)     m = (BITTA)(m | (mask & 2));
        r = rec_pos(fim, 2, m);   /* perfect extension items and then */
        if (r < 0) return -1;     /* find freq. item sets recursively */
        if (isr_report(fim->report) < 0)
          return -1;              /* report the current item set */
        isr_remove(fim->report,1);/* remove the item 2 again */
        }                           /* from the item set reporter */
        }
        if (n <= 3) {                 /* if only two items to process */
        memset(fim->wgts+1, 0, 7*sizeof(SUPP));  /* clear weights, */
        memset(fim->supps,  0, 3*sizeof(SUPP));  /* support and lists */
        memcpy(fim->ends, fim->btas, 3*sizeof(BITTA*));
        return 0;                   /* clear counters for items 0, 1, 2 */
        }                             /* and clear the transaction lists */
        if ((mask & 8)                /* if item 3 is frequent */
        &&  (fim->supps[3] >= fim->smin)) {
          r = isr_add(fim->report, fim->map[3], fim->supps[3]);
          if (r < 0) return -1;       /* add item 3 to the reporter */
        if (r > 0) {                /* if  item 3 needs processing, */
        m = 0;                    /* project/count for items 0 and 1 */
        fim->wgts[4]  = fim->wgts[12];
        fim->wgts[3]  = fim->wgts[11] +(fim->wgts[7] = fim->wgts[15]);
        fim->wgts[2]  = fim->wgts[10] +(fim->wgts[6] = fim->wgts[14]);
        fim->wgts[1]  = fim->wgts[ 9] +(fim->wgts[5] = fim->wgts[13]);
        fim->supps[0] = s = fim->wgts[3] +fim->wgts[1];
        if      (s >= fim->supps[3]) isr_addpex(fim->report, fim->map[0]);
        else if (s >= fim->smin)     m = (BITTA)(m | (mask & 1));
        fim->supps[1] = s = fim->wgts[3] +fim->wgts[2];
        if      (s >= fim->supps[3]) isr_addpex(fim->report, fim->map[1]);
        else if (s >= fim->smin)     m = (BITTA)(m | (mask & 2));
        fim->supps[2] = s = fim->wgts[7] +fim->wgts[6]
        + fim->wgts[5] +fim->wgts[4];
        if      (s >= fim->supps[3]) isr_addpex(fim->report, fim->map[2]);
        else if (s >= fim->smin)     m = (BITTA)(m | (mask & 4));
        r = rec_pos(fim, 3, m);   /* perfect extension items and then */
        if (r < 0) return -1;     /* find freq. item sets recursively */
        if (isr_report(fim->report) < 0)
          return -1;              /* report the current item set */
        isr_remove(fim->report,1);/* remove the item 3 again */
        }                           /* from the item set reporter */
        }
        memset(fim->wgts+1, 0, 15*sizeof(SUPP));  /* clear weights, */
        memset(fim->supps,  0,  4*sizeof(SUPP));  /* support and lists */
        memcpy(fim->ends, fim->btas, 4*sizeof(BITTA*));
        
        for (i = 4; i < n; i++) {     /* traverse the (remaining) items */
        s = fim->supps[i];          /* get the support of the item and */
        if (s < fim->smin) continue;/* skip infrequent/eliminated items */
        r = isr_add(fim->report, fim->map[i], s);
        if (r <  0) return -1;      /* add item i to the reporter */
        if (r <= 0) {               /* check if item i needs processing, */
        clear2(fim, i); continue;} /* otherwise delete its transactions */
        assert(mask & (1 << i));    /* item must not be excluded */
        if (fim->ends[i]-fim->btas[i] <= 1){ /* if only one transaction, */
        fim->ends[i] = fim->btas[i]; /* add all items as perfect exts. */
        fim->wgts[m = *fim->btas[i]] = 0;
        for (r = 0; (unsigned int)(1 << r) <= (unsigned int)m; r++)
          if (m & (1 << r)) isr_addpex(fim->report, fim->map[r]); }
        else {                      /* if more than one transaction */
        project(fim,i,mask, fim); /* project database to item i */
        count2(fim, i);            /* count the support of all items */
        m = filter (fim, i, s);   /* remove infrequent items and */
        r = rec_pos(fim, i, m);   /* perfect extension items and then */
        if (r < 0) return -1;     /* find freq. item sets recursively */
        }                           /* and check for a recursion error */
        if (isr_report(fim->report) < 0)
          return -1;                /* report the current item set */
        isr_remove(fim->report, 1); /* remove the item i again */
        }                             /* from the item set reporter */
        return 0;                     /* return 'ok' */
        }  /* rec_pos() */
        
        /*--------------------------------------------------------------------*/
        
        static int rec_neg (FIM16 *fim, int n, BITTA mask)
        {                               /* --- find frequent item sets */
        int   i, r;                   /* loop variable, error status */
        SUPP  s;                      /* item support */
        BITTA m;                      /* filtering mask */
        
        assert(fim && (n >= 0));      /* check the function arguments */
        for (i = n; --i >= 2; ) {     /* traverse the remaining items */
        s = fim->supps[i];          /* get the support of the item and */
        if (s < fim->smin) continue;/* skip infrequent/eliminated items */
        r = isr_add(fim->report, fim->map[i], s);
        if (r <  0) return -1;      /* add item i to the reporter */
        if (r <= 0) {               /* check if item i needs processing, */
        clear2(fim, i); continue;} /* otherwise delete its transactions */
        assert(mask & (1 << i));    /* item must not be excluded */
        if (fim->ends[i]-fim->btas[i] <= 1){ /* if only one transaction, */
        fim->ends[i] = fim->btas[i]; /* add all items as perfect exts. */
        fim->wgts[m = *fim->btas[i]] = 0;
        for (r = 0; (unsigned int)(1 << r) <= (unsigned int)m; r++)
          if (m & (1 << r)) isr_addpex(fim->report, fim->map[r]); }
        else {                      /* if more than one transaction */
        project(fim,i,mask,fim+1);/* project database to item i */
        count2(fim+1, i);          /* count th support of all items */
        m = filter (fim+1, i, s); /* remove infrequent items and */
        r = rec_neg(fim+1, i, m); /* prefect extension items and then */
        if (r < 0) return -1;     /* find freq. item sets recursively */
        }                           /* and check for a recursion error */
        if (isr_report(fim->report) < 0)
          return -1;                /* report the current item set */
        isr_remove(fim->report, 1); /* remove the item i again */
        }                             /* from the item set reporter */
        
        if ((mask & 2)                /* if item 1 is frequent */
        &&  (fim->supps[1] >= fim->smin)) {
          r = isr_add(fim->report, fim->map[1], fim->supps[1]);
          if (r < 0) return -1;       /* add item 1 to the reporter */
        if (r > 0) {                /* if item 1 needs processing, */
        if ((mask & 1)            /* if item 0 is frequent given 1 */
        &&  (fim->wgts[3] >= fim->smin)) {
          if (fim->wgts[3] >= fim->supps[1])
            isr_addpex(fim->report, fim->map[0]);
          else {                  /* check for a perfect extension */
        r = isr_add(fim->report, fim->map[0], fim->wgts[3]);
            if (r < 0) return -1; /* add item 0 to the reporter */
        if (r > 0) {          /* if item 0 needs processing */
        if (isr_report(fim->report) < 0)
          return -1;        /* report the current item set */
        isr_remove(fim->report, 1);
        }                     /* remove the item 0 again */
          }                       /* from the item set reporter */
        }
        if (isr_report(fim->report) < 0)
          return -1;              /* report the current item set */
        isr_remove(fim->report,1);/* remove the item 1 again */
        }                           /* from the item set reporter */
        }
        if ((mask & 1)                /* if item 0 is frequent */
        &&  (fim->supps[0] >= fim->smin)) {
          r = isr_add(fim->report, fim->map[0], fim->supps[0]);
          if (r < 0) return -1;       /* add item 0 to the reporter */
        if (r > 0) {                /* if item 0 needs processing */
        if (isr_report(fim->report) < 0)
          return -1;              /* report the current item set */
        isr_remove(fim->report,1);/* remove the item 0 again */
        }                           /* from the item set reporter */
        }
        memset(fim->wgts+1, 0, 3*sizeof(SUPP));  /* clear weights, */
        memset(fim->supps,  0, 2*sizeof(SUPP));  /* support and lists */
        memcpy(fim->ends, fim->btas, 2*sizeof(BITTA*));
        return 0;                     /* return 'ok' */
        }  /* rec_neg() */
        
        /*--------------------------------------------------------------------*/
        
        int m16_mine (FIM16 *fim)
        {                               /* --- find frequent item sets */
        int   r, i;                   /* error status, buffers */
        SUPP  s;                      /* support of current prefix */
        BITTA m;                      /* mask after item filtering, bit */
        
        assert(fim);                  /* check the function argument */
        if (fim->ttw <= 0) return 0;  /* if the machine is empty, abort */
        s = isr_supp(fim->report);    /* get min. support for perfect exts. */
        m = fim->tor;                 /* and bitwise or of all transactions */
        if (fim->wgts[m] >= s) {      /* if there is only one transaction, */
        fim->wgts[m] = 0;           /* remove it from the 16 items mach. */
        for (i = 0; (unsigned int)(1 << i) <= (unsigned int)m; i++)
          if (m & (1 << i))         /* all items are perfect extensions */
        isr_addpex(fim->report, fim->map[i]);
          i = hibit[m]; fim->ends[i] = fim->btas[i];
          fim->ttw = 0; fim->tor = 0; /* clear 16 items machine */
        return 0;                   /* and abort the function */
        }
        count2(fim, i = hibit[m]+1);   /* count the support of all items */
        m = filter(fim, i, s);        /* remove infreq. and perf. exts. */
        r = (fim->dir > 0)            /* find freq. item sets recursively */
        ? rec_pos(fim, i, m) : rec_neg(fim, i, m);
        fim->ttw = 0; fim->tor = 0;   /* clear the total transaction weight */
        return (r) ? r : m;           /* return error status or mask */
        }  /* m16_mine() */
        
        /*----------------------------------------------------------------------
        Preprocessor Definitions
        ----------------------------------------------------------------------*/
        /* --- target pattern types --- */
#define FPG_FREQ      ISR_FREQUENT  /* frequent item sets */
#define FPG_FREQUENT  ISR_FREQUENT  /* frequent item sets */
#define FPG_CLOSED    ISR_CLOSED    /* closed  frequent item sets */
#define FPG_MAXIMAL   ISR_MAXIMAL   /* maximal frequent item sets */
#define FPG_GENERAS   ISR_GENERAS   /* generators */
#define FPG_RULES     ISR_RULES     /* association rules */
        
        /* --- data preparation modes --- */
#define FPG_NORECODE  0x0001    /* do not sort and recode items */
#define FPG_NOFILTER  0x0002    /* do not filter transactions by size */
#define FPG_NOSORT    0x0004    /* do not sort items and transactions */
#define FPG_NOREDUCE  0x0008    /* do not reduce transactions */
#define FPG_NOPACK    0x0010    /* do not pack most frequent items */
#define FPG_SURR      (FPG_NORECODE|FPG_NOFILTER|FPG_NOREDUCE)
        
        /* --- evaluation measures --- */
        /* most definitions in ruleval.h */
#define FPG_LDRATIO   RE_FNCNT  /* binary log. of support quotient */
#define FPG_INVBXS    IST_INVBXS/* inval. eval. below exp. supp. */
        
        /* --- aggregation modes --- */
#define FPG_NONE      IST_NONE  /* no aggregation (use first value) */
#define FPG_FIRST     IST_FIRST /* no aggregation (use first value) */
#define FPG_MIN       IST_MIN   /* minimum of measure values */
#define FPG_MAX       IST_MAX   /* maximum of measure values */
#define FPG_AVG       IST_AVG   /* average of measure values */
        
        /* --- algorithm variants --- */
#define FPG_SIMPLE    0         /* simple  nodes (parent/link) */
#define FPG_COMPLEX   1         /* complex nodes (children/sibling) */
#define FPG_SINGLE    2         /* top-down processing on single tree */
#define FPG_TOPDOWN   3         /* top-down processing of the tree */
#define FPG_AUTO      4         /* automatic choice */
        
        /* --- operation modes --- */
#define FPG_FIM16     0x001f    /* use 16 items machine (bit rep.) */
#define FPG_PERFECT   0x0020    /* perfect extension pruning */
#define FPG_REORDER   0x0040    /* reorder items in cond. databases */
#define FPG_ORIGSUPP  0x0080    /* use original support definition */
#define FPG_TAIL      0x0100    /* head union tail pruning */
#define FPG_PREFMT    0x1000    /* pre-format integer numbers */
#define FPG_DEFAULT   (FPG_PERFECT|FPG_REORDER|FPG_TAIL|FPG_FIM16)
        /* in function fpgrowth() */
#define FPG_NOCLEAN   0         /* in debug version */
        
#define FPG_VERBOSE   INT_MIN   /* verbose message output */
        
        /*----------------------------------------------------------------------
        Type Definitions
----------------------------------------------------------------------*/
        typedef struct _fpgrowth        /* fpgrowth miner */
        FPGROWTH;                       /* (opaque structure) */
        
        /*----------------------------------------------------------------------
        Functions
        ----------------------------------------------------------------------*/
        FPGROWTH* fpg_create (double smin, ITEM zmin, ITEM zmax);
        void      fpg_delete (FPGROWTH *fpg, int deldar);
        int       fpg_data   (FPGROWTH *fpg, TABAG *tabag, int sort);
        int       fpg_report (FPGROWTH *fpg, ISREPORT *report);
        int       fpg_mine   (FPGROWTH *fpg, ITEM prune, int order);
        
        
        /*----------------------------------------------------------------------
        Preprocessor Definitions
        ----------------------------------------------------------------------*/
        
        /* --- error codes --- */
        /* error codes   0 to  -4 defined in tract.h */
#define E_STDIN      (-5)       /* double assignment of stdin */
#define E_OPTION     (-6)       /* unknown option */
#define E_OPTARG     (-7)       /* missing option argument */
#define E_ARGCNT     (-8)       /* too few/many arguments */
#define E_TARGET     (-9)       /* invalid target type */
#define E_SIZE      (-10)       /* invalid item set size */
#define E_SUPPORT   (-11)       /* invalid item set support */
#define E_CONF      (-12)       /* invalid confidence */
#define E_MEASURE   (-13)       /* invalid evaluation measure */
#define E_AGGMODE   (-14)       /* invalid aggregation mode */
#define E_VARIANT   (-16)       /* invalid algorithm variant */
        /* error codes -15 to -25 defined in tract.h */
        
#define COPYERR     ((TDNODE*)-1)
#define CLOCK(t)    ((t) = clock())
        
#define SEC_SINCE(t)  ((double)(clock()-(t)) /(double)CLOCKS_PER_SEC)
        
        /*----------------------------------------------------------------------
        Type Definitions
----------------------------------------------------------------------*/
        typedef struct fpnode {         /* --- frequent pattern tree node --- */
        ITEM          id;             /* item/head identifier */
        SUPP          supp;           /* support (weight of transactions) */
        struct fpnode *parent;        /* parent node (preceding item) */
        struct fpnode *succ;          /* successor node with same item */
        } FPNODE;                       /* (frequent pattern tree node) */
        
        typedef struct {                /* --- freq. pat. tree node list --- */
        ITEM     item;                /* associated item (item base code) */
        SUPP     supp;                /* support (weight of transactions) */
        FPNODE   *list;               /* list of nodes with this item */
        } FPHEAD;                       /* (frequent pattern tree head) */
        
        typedef struct {                /* --- frequent pattern tree --- */
        ITEM     cnt;                 /* number of items / heads */
        int      dir;                 /* processing direction */
        FIM16    *fim16;              /* 16-items machine */
        MEMSYS   *mem;                /* memory system for the nodes */
        FPNODE   root;                /* root node connecting trees */
        FPHEAD   heads[1];            /* header table (item lists) */
        } FPTREE;                       /* (frequent pattern tree) */
        
        typedef struct csnode {         /* --- children/sibling tree node --- */
        ITEM          id;             /* item/head identifier */
        SUPP          supp;           /* support (weight of transactions) */
        struct csnode *children;      /* list of child nodes */
        struct csnode *sibling;       /* successor node in sibling list */
        struct csnode *parent;        /* parent node (preceding item) */
        struct csnode *succ;          /* successor node with same item */
        } CSNODE;                       /* (children/sibling tree node) */
        
        typedef struct {                /* --- ch./sibling tree node list --- */
        ITEM     item;                /* associated item (item base code) */
        SUPP     supp;                /* support (weight of transactions) */
        CSNODE   *list;               /* list of nodes with this item */
        } CSHEAD;                       /* (children/sibling tree head) */
        
        typedef struct {                /* --- children/sibling tree --- */
        ITEM     cnt;                 /* number of items / heads */
        MEMSYS   *mem;                /* memory system for the nodes */
        CSNODE   root;                /* root node connecting trees */
        CSHEAD   heads[1];            /* header table (item lists) */
        } CSTREE;                       /* (children/sibling tree) */
        
        typedef struct tdnode {         /* --- top-down tree node --- */
        ITEM          id;             /* item/head identifier */
        SUPP          supp;           /* support (weight of transactions) */
        struct tdnode *children;      /* list of child nodes */
        struct tdnode *sibling;       /* successor node in sibling list */
        } TDNODE;                       /* (top-down tree node) */
        
        typedef struct {                /* --- top-down freq. pat. tree --- */
        ITEM     cnt;                 /* number of items / max. tree height */
        MEMSYS   *mem;                /* memory system for the nodes */
        TDNODE   *root;               /* root level of the tree */
        ITEM     items[1];            /* item identifier map */
        } TDTREE;                       /* (top-down tree) */
        
        struct _fpgrowth {              /* --- fpgrowth miner --- */
        int      target;              /* target type (e.g. closed/maximal) */
        double   smin;                /* minimum support of an item set */
        double   smax;                /* maximum support of an item set */
        SUPP     supp;                /* minimum support of an item set */
        SUPP     body;                /* minimum support of a rule body */
        double   conf;                /* minimum confidence of a rule */
        ITEM     zmin;                /* minimum size of a rule/item set */
        ITEM     zmax;                /* maximum size of a rule/item set */
        int      eval;                /* additional evaluation measure */
        int      agg;                 /* aggregation mode for eval. measure */
        double   thresh;              /* threshold for evaluation measure */
        int      algo;                /* variant of fpgrowth algorithm */
        int      mode;                /* search mode (e.g. pruning) */
        TABAG    *tabag;              /* transaction bag/multiset */
        ISREPORT *report;             /* item set reporter */
        int      dir;                 /* direction for item loops */
        ITEM     *set;                /* item set for projection */
        ITEM     *map;                /* item identifier map */
        SUPP     *cis;                /* conditional item support */
        FIM16    *fim16;              /* 16-items machine */
        ISTREE   *istree;             /* item set tree for fpg_tree() */
        };                              /* (fpgrowth miner) */
        
        typedef int FPGFN (FPGROWTH *fpg);
        
        
        /*----------------------------------------------------------------------
        Frequent Pattern Growth (simple nodes with only successor/parent)
        ----------------------------------------------------------------------*/
        
        static int add_simple (FPTREE *tree, const ITEM *ids, ITEM n, SUPP supp)
        {                               /* --- add an item set to the tree */
        ITEM   i;                     /* buffer for an item */
        FPNODE *c, *node;             /* to create new (child) nodes */
        
        assert(tree                   /* check the function arguments */
        &&    (ids || (n <= 0)) && (supp >= 0));
        node = &tree->root;           /* start at the root node and */
        while (1) {                   /* traverse the items of the set */
        node->supp += supp;         /* update the item set support */
        if (--n < 0) return 0;      /* if all items are processed, abort */
        i = *ids++;                 /* get the next item in the set */
        c = tree->heads[i].list;    /* and the corresponding list head */
        if (!c || (c->parent != node)) break;
        node = c;                   /* if last set is not matched, abort, */
        }                             /* otherwise go to the existing node */
        while (1) {                   /* traverse the new items */
        c = (FPNODE*)ms_alloc(tree->mem);
          if (!c) return -1;          /* allocate a new tree node */
        c->id     = i;              /* store the next item/list and */
        c->supp   = supp;           /* the support of the item set */
        c->parent = node;           /* connect to the parent node */
        c->succ   = tree->heads[i].list;   /* add the new node */
        tree->heads[i].list = node = c;    /* to the item list */
        if (--n < 0) return 1;      /* if there are no more items, */
        i = *ids++;                 /* abort the insertion loop, */
        }                             /* otherwise get the next item */
        }  /* add_simple() */
        
        /*--------------------------------------------------------------------*/
        
        static int proj_simple (FPGROWTH *fpg,
                                FPTREE *dst, FPTREE *src, ITEM id)
        {                               /* --- project a freq. pattern tree */
        ITEM   i, n;                  /* loop variables, item buffer */
        SUPP   pex;                   /* minimum support for perf. exts. */
        SUPP   *s;                    /* to sum the support values */
        ITEM   *map, *d;              /* to build the item map */
        FPHEAD *h;                    /* to access the node headers */
        FPNODE *node, *anc;           /* to traverse the tree nodes */
        
        assert(fpg                    /* check the function arguments */
        &&     dst && src && (id >= 0));
        memset(s = fpg->cis, 0, (size_t)id *sizeof(SUPP));
        for (node = src->heads[id].list; node; node = node->succ)
          for (anc = node->parent; anc->id >= 0; anc = anc->parent)
            s[anc->id] += node->supp; /* compute the conditional support */
        /* Using a two-dimensional table that is filled when a frequent */
        /* pattern tree is created proved to be slower than the above.  */
        pex = (fpg->mode & FPG_PERFECT) ? src->heads[id].supp : SUPP_MAX;
        map = fpg->map;               /* get perfect extension support */
        for (i = n = 0; i < id; i++){ /* traverse the items that */
        if (s[i] <  fpg->supp) {    /* precede the projection item, */
        map[i] = -1; continue; }  /* eliminate infrequent items and */
        if (s[i] >= pex) {          /* collect perfect extension items */
        map[i] = -1; isr_addpex(fpg->report,src->heads[i].item);continue;}
        map[i] = n;                 /* build the item identifier map */
        h = dst->heads +n++;        /* init. the destination header */
        h->item = src->heads[i].item;
        h->supp = s[i];             /* note the conditional item support */
        h->list = NULL;             /* and clear the node list */
        }
        if (n <= 0) return 0;         /* if the projection is empty, abort */
        dst->cnt       = n;           /* note the number of items and */
        dst->root.supp = 0;           /* init. root node and node heads */
        for (node = src->heads[id].list; node; node = node->succ) {
          d = map;                    /* traverse the item list */
        for (anc = node->parent; anc->id > TA_END; anc = anc->parent)
          if ((i = map[anc->id]) >= 0) *--d = i;
          if (add_simple(dst, d, (ITEM)(map-d), node->supp) < 0)
            return -1;                /* collect the non-eliminated items */
        }                             /* and add reduced trans. to the tree */
        return 1;                     /* return that result is not empty */
        }  /* proj_simple() */
        
        /*--------------------------------------------------------------------*/
        
        static int rec_simple (FPGROWTH *fpg, FPTREE *tree)
        {                               /* --- find item sets recursively */
        int    r;                     /* error status */
        ITEM   i, z;                  /* loop variables */
        FPHEAD *h;                    /* node list for current item */
        FPNODE *node, *anc;           /* to traverse the tree nodes */
        FPTREE *proj = NULL;          /* projected frequent pattern tree */
        ITEM   *s;                    /* to collect the tail items */
        
        assert(fpg && tree);          /* check the function arguments */
        if (fpg->mode & FPG_TAIL) {   /* if to use head union tail pruning */
        for (s = fpg->map, i = 0; i < tree->cnt; i++)
          *--s = tree->heads[i].item;/* collect the tail items */
        r = isr_tail(fpg->report, s, (ITEM)(fpg->map-s));
        if (r) return r;            /* if tail needs no processing, */
        }                             /* abort the recursion */
        if ((tree->cnt > 1)           /* if there is more than one item */
        &&  isr_xable(fpg->report,2)){/* and another item can be added */
        proj = (FPTREE*)malloc(sizeof(FPTREE)
                                 +(size_t)(tree->cnt-2) *sizeof(FPHEAD));
          if (!proj) return -1;       /* create a frequent pattern tree */
        proj->root.id   = TA_END;   /* of the maximally possible size */
        proj->root.succ = proj->root.parent = NULL;
        proj->dir = tree->dir;      /* initialize the root node and */
        proj->mem = tree->mem;      /* copy the processing direction */
        if (ms_push(tree->mem) < 0) { free(proj); return -1; }
        }                             /* note the current memory state */
        if (tree->dir > 0) { z = tree->cnt; i = 0; }
        else               { z = -1;        i = tree->cnt-1; }
        for (r = 0; i != z; i += tree->dir) {
          h = tree->heads +i;         /* traverse the (frequent) items */
        r = isr_add(fpg->report, h->item, h->supp);
        if (r <  0) break;          /* add current item to the reporter */
        if (r <= 0) continue;       /* check if item needs processing */
        node = h->list;             /* get the head of the item list */
        if (!node->succ) {          /* if projection would be a chain */
        for (anc = node->parent; anc->id > TA_END; anc = anc->parent) {
          isr_addpex(fpg->report, tree->heads[anc->id].item);
        } }                       /* add items as perfect extensions */
        else if (proj) {            /* if another item can be added */
        r = proj_simple(fpg, proj, tree, i);
          if (r > 0) r = rec_simple(fpg, proj);
          if (r < 0) break;         /* project frequent pattern tree and */
        }                           /* find freq. item sets recursively */
        r = isr_report(fpg->report);/* report the current item set */
        if (r < 0) break;           /* and check for an error */
        isr_remove(fpg->report, 1); /* remove the current item */
        }                             /* from the item set reporter */
        if (proj) {                   /* delete the created projection */
        free(proj); ms_pop(tree->mem); }
        return r;                     /* return the error status */
        }  /* rec_simple() */
        
        /*--------------------------------------------------------------------*/
        
        static int add_smp16 (FPTREE *tree, const ITEM *ids, ITEM n, SUPP supp)
        {                               /* --- add an item set to the tree */
        ITEM   i;                     /* buffer for an item */
        FPNODE *c, *node;             /* to create new (child) nodes */
        
        assert(tree                   /* check the function arguments */
        &&    (ids || (n <= 0)) && (supp >= 0));
        node = &tree->root;           /* start at the root node and */
        node->supp += supp;           /* update the item set support */
        if (--n < 0) return 0;        /* if there are no items, abort */
        if (*ids < 0) {               /* if there are packed items, */
        i = *ids++;                 /* get and skip the packed items and */
        if (tree->dir > 0)          /* add them to the 16-items machine */
        m16_add(tree->fim16, (BITTA)(i & ~TA_END), supp);
        tree->heads[0].item |= i;   /* compute union of packed items */
        node = tree->heads[0].list; /* get the corresponding list head */
        if (node && (node->id == i))
          node->supp += supp;       /* if node exists, add the support */
        else {                      /* if no proper node exists */
        c = (FPNODE*)ms_alloc(tree->mem);
          if (!c) return -1;        /* allocate a new tree node */
        c->id     = i;            /* store the packed items and */
        c->supp   = supp;         /* the support of the item set */
        c->parent = &tree->root;  /* connect to the root node */
        c->succ   = node;         /* add the new node to the list */
        tree->heads[0].list = node = c;    /* for the packed items */
        }                           /* (all packed items are in list 0) */
        if (--n < 0) return 0;      /* if there are no other items, abort */
        }
        while (1) {                   /* traverse the items of the set */
        i = *ids++;                 /* get the next item in the set */
        c = tree->heads[i].list;    /* and the corresponding list head */
        if (!c || (c->parent != node)) break;
        node = c;                   /* if last set is not matched, abort, */
        node->supp += supp;         /* otherwise go to the existing node, */
        if (--n < 0) return 0;      /* update the item set support, and */
        }                             /* if all items are processed, abort */
        while (1) {                   /* traverse the new items */
        c = (FPNODE*)ms_alloc(tree->mem);
          if (!c) return -1;          /* allocate a new tree node */
        c->id     = i;              /* store the next item and */
        c->supp   = supp;           /* the support of the item set */
        c->parent = node;           /* connect to the parent node */
        c->succ   = tree->heads[i].list;   /* add the new node */
        tree->heads[i].list = node = c;    /* to the item list */
        if (--n < 0) return 1;      /* if there are no more items, */
        i = *ids++;                 /* abort the insertion loop, */
        }                             /* otherwise get the next item */
        }  /* add_smp16() */
        
        /*--------------------------------------------------------------------*/
        
        static int proj_smp16 (FPGROWTH *fpg, FPTREE *dst, FPTREE *src,
                               ITEM id, ITEM mask)
        {                               /* --- project a freq. pattern tree */
        ITEM   i, n;                  /* loop variables, item buffer */
        SUPP   pex;                   /* minimum support for perf. exts. */
        SUPP   *s;                    /* to sum the support values */
        ITEM   *map, *d;              /* to build the item map */
        FPHEAD *h;                    /* to access the node heads */
        FPNODE *node, *anc;           /* to traverse the tree nodes */
        
        assert(fpg                    /* check the function arguments */
        &&     dst && src && (id >= 0));
        memset(s = fpg->cis, 0, (size_t)id *sizeof(SUPP));
        for (node = src->heads[id].list; node; node = node->succ)
          for (anc = node->parent; anc->id >= 0; anc = anc->parent)
            s[anc->id] += node->supp; /* compute the conditional support */
        /* Using a two-dimensional table that is filled when a frequent */
        /* pattern tree is created proved to be slower than the above.  */
        pex = (fpg->mode & FPG_PERFECT) ? src->heads[id].supp : SUPP_MAX;
        map = fpg->map;               /* get perfect extension support */
        h   = dst->heads;             /* always keep head for packed items */
        h->item = TA_END; h->supp = 0; h->list = NULL;
        for (i = n = 1; i < id; i++){ /* traverse the items that */
        if (s[i] <  fpg->supp) {    /* precede the projection item, */
        map[i] = -1; continue; }  /* eliminate infrequent items and */
        if (s[i] >= pex) {          /* collect perfect extension items */
        map[i] = -1; isr_addpex(fpg->report,src->heads[i].item);continue;}
        map[i] = n;                 /* build the item identifier map */
        h = dst->heads +n++;        /* init. the destination header */
        h->item = src->heads[i].item;
        h->supp = s[i];             /* note the conditional item support */
        h->list = NULL;             /* and clear the node list */
        }
        if (n <= 0) return 0;         /* if the projection is empty, abort */
        dst->cnt       = n;           /* note the number of items and */
        dst->root.supp = 0;           /* init. root node and node heads */
        for (node = src->heads[id].list; node; node = node->succ) {
          d = map;                    /* traverse the item list */
        for (anc = node->parent; anc->id > TA_END; anc = anc->parent) {
          i = anc->id;              /* traverse path to the root */
        if (i < 0) { if ((i &= mask)) *--d = i | TA_END; }
        else if ((i = map[i]) >= 0)   *--d = i;
        }                           /* collect the non-eliminated items */
        if (add_smp16(dst, d, (ITEM)(map-d), node->supp) < 0)
          return -1;                /* add reduced trans. to the tree */
        }
        return 1;                     /* return that result is not empty */
        }  /* proj_smp16() */
        
        /*--------------------------------------------------------------------*/
        
        static int rec_smp16 (FPGROWTH *fpg, FPTREE *tree)
        {                               /* --- find item sets recursively */
        int    r;                     /* error status */
        ITEM   i, k, z;               /* loop variables */
        ITEM   mask;                  /* mask for packed items to keep */
        FPHEAD *h;                    /* node list for current item */
        FPNODE *node, *anc;           /* to traverse the tree nodes */
        FPTREE *proj = NULL;          /* projected frequent pattern tree */
        ITEM   *s;                    /* to collect the tail items */
        
        assert(fpg && tree);          /* check the function arguments */
        if (fpg->mode & FPG_TAIL) {   /* if to use head union tail pruning */
        s = fpg->map; i = k = 0;    /* get the item set buffer */
        if (tree->heads[0].item < 0) {
          for (i++, z = 0; z < 16; z++)
            if (tree->heads[0].item & (1 << z))
              *--s = z;             /* if there are packed items, */
        }                           /* first collect these tail items */
        for ( ; i < tree->cnt; i++) /* and then the other tail items */
        *--s = tree->heads[i].item;
        r = isr_tail(fpg->report, s, (ITEM)(fpg->map-s));
        if (r) return r;            /* if tail needs no processing, */
        }                             /* abort the recursion */
        if ((tree->cnt > 1)           /* if there is more than one item */
        &&  isr_xable(fpg->report,2)){/* and another item can be added */
        proj = (FPTREE*)malloc(sizeof(FPTREE)
                                 +(size_t)(tree->cnt-2) *sizeof(FPHEAD));
          if (!proj) return -1;       /* create a frequent pattern tree */
        proj->root.id   = TA_END;   /* of the maximally possible size */
        proj->root.succ = proj->root.parent = NULL;
        proj->dir   = tree->dir;    /* initialize the root node and */
        proj->fim16 = tree->fim16;  /* copy the processing direction */
        proj->mem   = tree->mem;    /* and the 16-items machine */
        if (ms_push(tree->mem) < 0) { free(proj); return -1; }
        }                             /* note the current memory state */
        mask = ITEM_MAX;              /* init. the packed item mask */
        if (tree->dir > 0) { z = tree->cnt; i = 0; }
        else               { z = -1;        i = tree->cnt-1; }
        for (r = 0; i != z; i += tree->dir) {
          h = tree->heads +i;         /* traverse the (frequent) items */
        if (h->item < 0) {          /* if to use a 16-items machine */
        if (tree->dir < 0)        /* if downward processing direction */
        for (node = h->list; node; node = node->succ)
          m16_add(tree->fim16, (BITTA)(node->id & ~TA_END), node->supp);
        r = m16_mine(tree->fim16);/* add bit-rep. transaction prefixes */
        if (r < 0) break;         /* to the 16-items machine and mine */
        mask = r; continue;       /* get the packed items mask */
        }                           /* and go to the next item list */
        r = isr_add(fpg->report, h->item, h->supp);
        if (r <  0) break;          /* add current item to the reporter */
        if (r <= 0) continue;       /* check if item needs processing */
        node = h->list;             /* get the head of the item list */
        if (!node->succ) {          /* if projection would be a chain */
        for (anc = node->parent; anc->id > TA_END; anc = anc->parent) {
          k = anc->id;            /* traverse the list of ancestors */
        if (k >= 0) isr_addpex  (fpg->report, tree->heads[k].item);
        else        isr_addpexpk(fpg->report, k);
        } }                       /* add items as perfect extensions */
        else if (proj) {            /* if another item can be added */
        r = proj_smp16(fpg, proj, tree, i, mask);
          if (r > 0) r = rec_smp16(fpg, proj);
          if (r < 0) break;         /* project frequent pattern tree and */
        }                           /* find freq. item sets recursively */
        r = isr_report(fpg->report);/* report the current item set */
        if (r < 0) break;           /* and check for an error */
        isr_remove(fpg->report, 1); /* remove the current item */
        }                             /* from the item set reporter */
        if (proj) {                   /* delete the created projection */
        free(proj); ms_pop(tree->mem); }
        return r;                     /* return the error status */
        }  /* rec_smp16() */
        
        /*--------------------------------------------------------------------*/
        
        int fpg_simple (FPGROWTH *fpg)
        {                               /* --- search for frequent item sets */
        int        r = 0;             /* result of recursion/functions */
        ITEM       i, k, m;           /* loop variable, number of items */
        TID        j, n;              /* loop variable, number of trans. */
        SUPP       pex;               /* minimum support for perf. exts. */
        TRACT      *t;                /* to traverse the transactions */
        ITEM       *s, *d;            /* to build the item maps */
        const ITEM *p;                /* to traverse transaction items */
        const SUPP *f;                /* item frequencies in trans. bag */
        FPTREE     *tree;             /* created frequent pattern tree */
        FPHEAD     *h;                /* to traverse the item heads */
        
        assert(fpg);                  /* check the function arguments */
        pex = tbg_wgt(fpg->tabag);    /* check against the minimum support */
        if (fpg->supp > pex) return 0;/* and get minimum for perfect exts. */
        if (!(fpg->mode & FPG_PERFECT)) pex = SUPP_MAX;
        n = tbg_cnt(fpg->tabag);      /* get the number of transactions */
        k = tbg_itemcnt(fpg->tabag);  /* and check the number of items */
        if (k <= 0) return isr_report(fpg->report);
        f = tbg_ifrqs(fpg->tabag, 0); /* get the item frequencies */
        if (!f) return -1;            /* in the transaction bag */
        s = fpg->set = (ITEM*)malloc((size_t)(k+k) *sizeof(ITEM)
                                       +(size_t) k    *sizeof(SUPP));
        if (!s) return -1;            /* create item and support arrays */
        fpg->map = d = s+k;           /* note item map and set buffer */
        fpg->cis = (SUPP*)(d+k);      /* and the item support array */
        if (!(fpg->mode & FPG_FIM16)) m = 0;   /* if 16-items machine, */
        else {      d[0] = s[0] = 0;  m = 1; } /* always keep packed items */
        for (i = m; i < k; i++) {     /* build the item identifier map */
        if (f[i] <  fpg->supp) { d[i] = -1; continue; }
        if (f[i] >= pex)       { isr_addpex(fpg->report, i);
          d[i] = -1; continue; }
        d[i] = m; s[m++] = i;       /* eliminate infrequent items and */
        }                             /* collect perfect extension items */
        if (m <= 0) {                 /* check whether there are items left */
        r = isr_report(fpg->report); free(fpg->set); return r; }
        fpg->dir = (fpg->target & (ISR_CLOSED|ISR_MAXIMAL)) ? -1 : +1;
        tree = (FPTREE*)malloc(sizeof(FPTREE) +(size_t)(m-1) *sizeof(FPHEAD));
        if (!tree) { free(fpg->set); return -1; }
        tree->cnt = k = m;            /* allocate the base tree structure */
        tree->dir = fpg->dir;         /* and initialize its fields */
        tree->mem = ms_create(sizeof(FPNODE), 65535);
        if (!tree->mem) { free(tree); free(fpg->set); return -1; }
        tree->root.id   = TA_END;     /* create memory system for the nodes */
        tree->root.supp = 0;          /* and initialize the root node */
        tree->root.succ = tree->root.parent = NULL;
        for (i = 0; i < k; i++) {     /* initialize the header table */
        h = tree->heads+i; h->supp = f[h->item = s[i]]; h->list = NULL; }
        if (fpg->mode & FPG_FIM16) {  /* if to use a 16-items machine */
        tree->fim16 = m16_create(fpg->dir, fpg->supp, fpg->report);
          if (!tree->fim16) { ms_delete(tree->mem);
            free(tree); free(fpg->set); return -1; }
          tree->heads[0].item = TA_END;   /* create a 16-items machine */
        for (j = n; --j >= 0; ) {   /* traverse the transactions and */
        t = tbg_tract(fpg->tabag, j);   /* collect non-elim. items */
        for (k = 0, p = ta_items(t); *p > TA_END; p++) {
          if      ((m = *p)   <  0) s[k++] = m;
          else if ((m = d[m]) >= 0) s[k++] = m;
        }                         /* (packed items are always copied) */
        r = add_smp16(tree, s, k, ta_wgt(t));
        if (r < 0) break;         /* add the reduced transaction */
        }                           /* to the frequent pattern tree */
        if (r >= 0) {               /* if freq. pattern tree was built, */
        r = rec_smp16(fpg, tree); /* find freq. item sets recursively */
        if (r >= 0) r = isr_report(fpg->report);
        }                           /* finally report the empty item set */
        m16_delete(tree->fim16); }  /* delete the 16-items machine */
        else {                        /* if not to use a 16-items machine */
        for (j = n; --j >= 0; ) {   /* traverse the transactions and */
        t = tbg_tract(fpg->tabag, j);   /* collect non-elim. items */
        for (k = 0, p = ta_items(t); *p > TA_END; p++)
          if ((m = d[*p]) >= 0) s[k++] = m;
          r = add_simple(tree, s, k, ta_wgt(t));
          if (r < 0) break;         /* add the reduced transaction */
        }                           /* to the frequent pattern tree */
        if (r >= 0) {               /* if freq. pattern tree was built, */
        r = rec_simple(fpg,tree); /* find freq. item sets recursively */
        if (r >= 0) r = isr_report(fpg->report);
        }                           /* report the empty item set */
        }
        ms_delete(tree->mem);         /* delete the memory mgmt. system */
        free(tree); free(fpg->set);   /* and the frequent pattern tree */
        return r;                     /* return the error status */
        }  /* fpg_simple() */
        
        /*----------------------------------------------------------------------
        Frequent Pattern Growth (complex nodes with children/sibling)
        ----------------------------------------------------------------------*/
        
        static int add_cmplx (CSTREE *tree, const ITEM *ids, ITEM n, SUPP supp)
        {                               /* --- add an item set to the tree */
        ITEM   i;                     /* buffer for an item */
        CSNODE *node;                 /* to traverse the nodes */
        CSNODE **p, *c;               /* to create new nodes */
        
        assert(tree                   /* check the function arguments */
        &&    (ids || (n <= 0)) && (supp >= 0));
        node = &tree->root;           /* start at the root node and */
        while (1) {                   /* traverse the items of the set */
        node->supp += supp;         /* update the item set support */
        if (--n < 0) return 0;      /* if all items are processed, abort */
        i = *ids++;                 /* get the next item in the set and */
        p = &node->children;        /* traverse the list of children */
        while (*p && ((*p)->id < i)) p = &(*p)->sibling;
        if (!(c = *p) || (c->id != i)) break;
        node = c;                   /* find the item/insertion position */
        }                             /* and if item does not exist, abort */
        c = (CSNODE*)ms_alloc(tree->mem);
        if (!c) return -1;            /* create a new prefix tree node */
        c->id      = i;               /* store the current item and */
        c->supp    = supp;            /* the support of the item set */
        c->parent  = node;            /* connect to the parent node */
        c->sibling = *p;              /* insert the created node into */
        c->succ    = tree->heads[i].list;      /* the sibling list and */
        tree->heads[i].list = node = *p = c;   /* into the item list */
        while (--n >= 0) {            /* traverse the rest of the items */
        node->children = c = (CSNODE*)ms_alloc(tree->mem);
          if (!c) return -1;          /* create a new prefix tree node */
        c->id  = i = *ids++;        /* store the next item and */
        c->supp    = supp;          /* the support of the item set */
        c->parent  = node;          /* connect to the parent node */
        c->sibling = NULL;          /* there are no siblings yet */
        c->succ    = tree->heads[i].list;
        tree->heads[i].list = node = c;
        }                             /* insert node into the item list */
        node->children = NULL;        /* last created node is a leaf */
        return 1;                     /* return that nodes were added */
        }  /* add_cmplx() */
        
        /*--------------------------------------------------------------------*/
        
        static int proj_cmplx (FPGROWTH *fpg, CSTREE *dst, CSTREE *src, ITEM id)
        {                               /* --- project a freq. pattern tree */
        int    r;                     /* result of function call */
        ITEM   i, n;                  /* loop variables, identifier buffer */
        SUPP   pex;                   /* minimum support for perfect exts. */
        SUPP   *s;                    /* to sum the support values */
        ITEM   *map, *d;              /* to build the item map */
        CSHEAD *h;                    /* to traverse the item heads */
        CSNODE *node, *anc;           /* to traverse the tree nodes */
        
        assert(fpg                    /* check the function arguments */
        &&     dst && src && (id >= 0));
        memset(s = fpg->cis, 0, (size_t)id *sizeof(SUPP));
        for (node = src->heads[id].list; node; node = node->succ)
          for (anc = node->parent; anc->id >= 0; anc = anc->parent)
            s[anc->id] += node->supp; /* compute the conditional support */
        /* Using a two-dimensional table that is filled when a frequent */
        /* pattern tree is created proved to be slower than the above.  */
        pex = (fpg->mode & FPG_PERFECT) ? src->heads[id].supp : SUPP_MAX;
        map = fpg->map;               /* get perfect extension support */
        for (i = n = 0; i < id; i++){ /* traverse the items that */
        if (s[i] <  fpg->supp) {    /* precede the projection item, */
        map[i] = -1; continue; }  /* eliminate infrequent items and */
        if (s[i] >= pex) {          /* collect perfect extension items */
        map[i] = -1; isr_addpex(fpg->report,src->heads[i].item);continue;}
        map[i] = n;                 /* build the item identifier map */
        h = dst->heads +n++;        /* init. the destination item list */
        h->item = src->heads[i].item;
        h->supp = s[i];             /* note the conditional item support */
        h->list = NULL;             /* and clear the node list */
        }
        if (n <= 0) return 0;         /* if the projection is empty, abort */
        dst->cnt = n;                 /* note the number of items */
        if ((n <= 16) && fpg->fim16){ /* if at most 16 items left, */
        h = dst->heads;             /* traverse the remaining items */
        for (i = 0; i < n; i++)     /* and set the item identifier map */
        m16_setmap(fpg->fim16, i, h[i].item);
        for (node = src->heads[id].list; node; node = node->succ) {
          n = 0;                    /* traverse the item list */
        for (anc = node->parent; anc->id >= 0; anc = anc->parent)
          if ((i = map[anc->id]) >= 0) n |= 1 << i;
          m16_add(fpg->fim16, (BITTA)(n & ~TA_END), node->supp);
        }                           /* add bit-represented transactions */
        r = m16_mine(fpg->fim16);   /* to the 16-items machine and mine */
        return (r < 0) ? r : 0;     /* return 'no projection created' */
        }
        dst->root.supp     = 0;       /* init. the root node support */
        dst->root.children = NULL;    /* and clear the child list */
        for (node = src->heads[id].list; node; node = node->succ) {
          d = map;                    /* traverse the item list */
        for (anc = node->parent; anc->id >= 0; anc = anc->parent)
          if ((i = map[anc->id]) >= 0) *--d = i;
          if (add_cmplx(dst, d, (ITEM)(map-d), node->supp) < 0)
            return -1;                /* collect the non-eliminated items */
        }                             /* and add reduced trans. to the tree */
        return 1;                     /* return 'projection created' */
        }  /* proj_cmplx() */
        
        /*--------------------------------------------------------------------*/
        
        static int add_reord (CSTREE *tree, ITEM *flags, ITEM n, SUPP supp)
        {                               /* --- add an item set to the tree */
        ITEM   i;                     /* buffer for an item */
        CSNODE *node;                 /* to traverse the nodes */
        CSNODE **p, *c;               /* to create new nodes */
        
        assert(tree                   /* check the function arguments */
        &&    (n >= 0) && (supp >= 0));
        node = &tree->root;           /* start at the root node and */
        node->supp += supp;           /* update the empty set support */
        if (n <= 0) return 0;         /* if transaction is empty, abort */
        for (i = 0; 1; i++) {         /* traverse the items in the tree */
        if (!flags[i]) continue;    /* if item is not in set, skip it */
        flags[i] = 0;               /* clear the containment flag */
        p = &node->children;        /* traverse the list of children */
        while (*p && ((*p)->id < i)) p = &(*p)->sibling;
        if (!(c = *p) || (c->id != i)) break;
        node = c;                   /* find the item/insertion position */
        node->supp += supp;         /* if the item does not exist, abort */
        if (--n <= 0) return 0;     /* otherwise update the support */
        }                             /* and check for last item */
        c = (CSNODE*)ms_alloc(tree->mem);
        if (!c) return -1;            /* create a new prefix tree node */
        c->id      = i;               /* store the current item and */
        c->supp    = supp;            /* the support of the item set */
        c->parent  = node;            /* connect to the parent node */
        c->sibling = *p;              /* insert the created node into */
        c->succ    = tree->heads[i].list;      /* the sibling list and */
        tree->heads[i].list = node = *p = c;   /* into the item list */
        if (--n <= 0) { node->children = NULL; return 1; }
        while (1) {                   /* traverse the remaining items */
        if (!flags[++i]) continue;  /* if item is not in set, skip it */
        flags[i] = 0;               /* clear the containment flag */
        node->children = c = (CSNODE*)ms_alloc(tree->mem);
        if (!c) return -1;          /* create a new prefix tree node */
        c->id      = i;             /* store the next item and */
        c->supp    = supp;          /* the support of the item set */
        c->parent  = node;          /* connect to the parent node */
        c->sibling = NULL;          /* there are no siblings yet */
        c->succ    = tree->heads[i].list;  /* insert the new node */
        tree->heads[i].list = node = c;    /* into the item list */
        if (--n <= 0) break;        /* check for last item */
        }
        node->children = NULL;        /* last created node is a leaf */
        return 1;                     /* return that nodes were added */
        }  /* add_reord() */
        
        /*--------------------------------------------------------------------*/
        
        static int proj_reord (FPGROWTH *fpg, CSTREE *dst, CSTREE *src, ITEM id)
        {                               /* --- project a freq. pattern tree */
        int    r;                     /* result of function calls */
        ITEM   i, k, n;               /* loop variables, item buffer */
        SUPP   pex;                   /* minimum support for perfect exts. */
        SUPP   *s;                    /* to sum the support values */
        ITEM   *map, *d;              /* to build the item map */
        CSHEAD *h;                    /* to traverse the item heads */
        CSNODE *node, *anc;           /* to traverse the tree nodes */
        
        assert(fpg                    /* check the function arguments */
        &&     dst && src && (id >= 0));
        memset(s = fpg->cis, 0, (size_t)id *sizeof(SUPP));
        for (node = src->heads[id].list; node; node = node->succ)
          for (anc = node->parent; anc->id >= 0; anc = anc->parent)
            s[anc->id] += node->supp; /* compute the conditional support */
        /* Using a two-dimensional table that is filled when a frequent */
        /* pattern tree is created proved to be slower than the above.  */
        pex = (fpg->mode & FPG_PERFECT) ? src->heads[id].supp : SUPP_MAX;
        map = fpg->set;               /* get perfect extension support */
        for (d = map +id, i = n = 0; i < id; i++) {
          if (s[i] <  fpg->supp) {    /* traverse items and their support */
        map[i] = -1; continue; }  /* eliminate infrequent items and */
        if (s[i] >= pex) {          /* collect perfect extension items */
        map[i] = -1; isr_addpex(fpg->report,src->heads[i].item);continue;}
        d[n++] = i;                 /* collect the remaining items */
        }                             /* in a temporary buffer */
        if (n <= 0) return 0;         /* if the projection is empty, abort */
        dst->cnt = n;                 /* note the number of items */
        i2i_qsort(d, (size_t)n,-1, s); /* sort the rem. items descendingly */
        for (i = 0; i < n; i++) {     /* traverse the sorted items */
        h = dst->heads +i;          /* init. the destination item list */
        h->item = src->heads[k = d[i]].item;
        h->supp = s[k]; map[k] = i; /* build the item identifier maps */
        h->list = NULL;             /* and store the item support */
        }
        if ((n <= 16) && fpg->fim16){ /* if at most 16-items left */
        h = dst->heads;             /* traverse the remaining items */
        for (i = 0; i < n; i++)     /* and set the item identifier map */
        m16_setmap(fpg->fim16, i, h[i].item);
        for (node = src->heads[id].list; node; node = node->succ) {
          n = 0;                    /* traverse the item list */
        for (anc = node->parent; anc->id >= 0; anc = anc->parent)
          if ((i = map[anc->id]) >= 0) n |= 1 << i;
          m16_add(fpg->fim16, (BITTA)(n & ~TA_END), node->supp);
        }                           /* add bit-represented transactions */
        r = m16_mine(fpg->fim16);   /* to the 16-items machine and mine */
        return (r < 0) ? r : 0;     /* return 'no projection created' */
        }
        dst->root.supp     = 0;       /* init. the root node support */
        dst->root.children = NULL;    /* and clear the child list */
        /* this version is slightly faster */
        memset(d, 0, (size_t)id *sizeof(ITEM));
        for (node = src->heads[id].list; node; node = node->succ) {
          k = 0;                      /* traverse the item list */
        for (anc = node->parent; anc->id >= 0; anc = anc->parent)
          if ((i = map[anc->id]) >= 0) k += d[i] = 1;
          if (add_reord(dst, d, k, node->supp) < 0)
            return -1;                /* collect the non-eliminated items */
        }                             /* and add reduced trans. to the tree */
        return 1;                     /* return 'projection created' */
        }  /* proj_reord() */
        
        /*--------------------------------------------------------------------*/
        
        static int rec_cmplx (FPGROWTH *fpg, CSTREE *tree)
        {                               /* --- find item sets recursively */
        int    r;                     /* error status */
        ITEM   i, z;                  /* loop variables */
        CSHEAD *h;                    /* node list for current item */
        CSNODE *node;                 /* to traverse the tree nodes */
        CSTREE *proj = NULL;          /* projected frequent pattern tree */
        ITEM   *s;                    /* to collect the tail items */
        
        assert(fpg && tree);          /* check the function arguments */
        if (fpg->mode & FPG_TAIL) {   /* if to use head union tail pruning */
        for (s = fpg->map, i = 0; i < tree->cnt; i++)
          *--s = tree->heads[i].item;  /* collect the tail items */
        r = isr_tail(fpg->report, s, tree->cnt);
        if (r) return r;            /* if tail needs no processing, */
        }                             /* abort the recursion */
        if ((tree->cnt > 1)           /* if there is more than one item */
        &&  isr_xable(fpg->report,2)){/* and another item can be added */
        proj = (CSTREE*)malloc(sizeof(CSTREE)
                                 +(size_t)(tree->cnt-2) *sizeof(CSHEAD));
          if (!proj) return -1;       /* create a frequent pattern tree */
        proj->root.id   = TA_END;   /* of the maximally possible size */
        proj->root.succ = proj->root.parent = proj->root.sibling = NULL;
        proj->mem = tree->mem;      /* initialize the root node */
        if (ms_push(tree->mem) < 0) { free(proj); return -1; }
        }                             /* note the current memory state */
        if (fpg->dir > 0) { z = tree->cnt; i = 0; }
        else              { z = -1;        i = tree->cnt-1; }
        for (r = 0; i != z; i += fpg->dir) {
          h = tree->heads +i;         /* traverse the frequent items */
        r = isr_add(fpg->report, h->item, h->supp);
        if (r <  0) break;          /* add current item to the reporter */
        if (r <= 0) continue;       /* check if item needs processing */
        if (!h->list->succ) {       /* if projection would be a chain */
        for (node = h->list->parent; node->id >= 0; ) {
          isr_addpex(fpg->report, tree->heads[node->id].item);
          node = node->parent;    /* traverse the list of ancestors */
        } }                       /* and add them as perfect exts. */
        else if (proj) {            /* if another item can be added */
        r = (fpg->mode & FPG_REORDER)
          ? proj_reord(fpg, proj, tree, i)
            : proj_cmplx(fpg, proj, tree, i);
          if (r > 0) r = rec_cmplx(fpg, proj);
          if (r < 0) break;         /* project frequent pattern tree and */
        }                           /* find freq. item sets recursively */
        r = isr_report(fpg->report);/* report the current item set */
        if (r < 0) break;           /* and check for an error */
        isr_remove(fpg->report, 1); /* remove the current item */
        }                             /* from the item set reporter */
        if (proj) {                   /* delete the created projection */
        free(proj); ms_pop(tree->mem); }
        return r;                     /* return the error status */
        }  /* rec_cmplx() */
        
        /*--------------------------------------------------------------------*/
        
        int fpg_cmplx (FPGROWTH *fpg)
        {                               /* --- search for frequent item sets */
        int        r = 0;             /* result of recursion/functions */
        ITEM       i, k, m;           /* loop variable, number of items */
        TID        j, n;              /* loop variable, number of trans. */
        SUPP       pex;               /* minimum support for perfect exts. */
        ITEM       *s, *d;            /* to build the item maps */
        const ITEM *p;                /* to traverse transaction items */
        const SUPP *f;                /* item frequencies in trans. bag */
        TRACT      *t;                /* to traverse the transactions */
        CSTREE     *tree;             /* created frequent pattern tree */
        CSHEAD     *h;                /* to traverse the item heads */
        
        assert(fpg);                  /* check the function arguments */
        pex = tbg_wgt(fpg->tabag);    /* check against the minimum support */
        if (fpg->supp > pex) return 0;/* and get minimum for perfect exts. */
        if (!(fpg->mode & FPG_PERFECT)) pex = SUPP_MAX;
        n = tbg_cnt(fpg->tabag);      /* get the number of transactions */
        k = tbg_itemcnt(fpg->tabag);  /* and check the number of items */
        if (k <= 0) return isr_report(fpg->report);
        f = tbg_ifrqs(fpg->tabag, 0); /* get the item frequencies */
        if (!f) return -1;            /* in the transaction bag */
        s = fpg->set = (ITEM*)malloc((size_t)(k+k) *sizeof(ITEM)
                                       +(size_t) k    *sizeof(SUPP));
        if (!s) return -1;            /* create item and support arrays */
        fpg->map = d = s+k;           /* note item map and set buffer */
        fpg->cis = (SUPP*)(d+k);      /* and the item support array */
        for (i = m = 0; i < k; i++) { /* build the item identifier map */
        if (f[i] <  fpg->supp) { d[i] = -1; continue; }
        if (f[i] >= pex)       { isr_addpex(fpg->report, i);
          d[i] = -1; continue; }
        d[i] = m; s[m++] = i;       /* eliminate infrequent items and */
        }                             /* collect perfect extension items */
        if (m <= 0) {                 /* check whether there are items left */
        r = isr_report(fpg->report); free(fpg->set); return r; }
        fpg->dir = (fpg->target & (ISR_CLOSED|ISR_MAXIMAL)) ? -1 : +1;
        tree = (CSTREE*)malloc(sizeof(CSTREE) +(size_t)(m-1) *sizeof(CSHEAD));
        if (!tree) { free(fpg->set); return -1; }
        tree->cnt = k = m;            /* allocate the base tree structure */
        tree->mem = ms_create(sizeof(CSNODE), 65535);
        if (!tree->mem) { free(tree); free(fpg->set); return -1; }
        tree->root.id      = TA_END;  /* create memory system for the nodes */
        tree->root.supp    = 0;       /* and initialize the root node */
        tree->root.sibling = tree->root.children = NULL;
        tree->root.succ    = tree->root.parent   = NULL;
        for (i = 0; i < k; i++) {     /* initialize the header table */
        h = tree->heads+i; h->supp = f[h->item = s[i]]; h->list = NULL; }
        fpg->fim16 = NULL;            /* default: no 16-items machine */
        if (fpg->mode & FPG_FIM16) {  /* if to use a 16-items machine */
        fpg->fim16 = m16_create(fpg->dir, fpg->supp, fpg->report);
          if (!fpg->fim16) { ms_delete(tree->mem);
            free(tree); free(fpg->set); return -1; }
        }                             /* create a 16-items machine */
        for (j = n; --j >= 0; ) {     /* traverse the transactions and */
        t = tbg_tract(fpg->tabag,j);/* collect the non-eliminated items */
        for (k = 0, p = ta_items(t); *p > TA_END; p++)
          if ((m = d[*p]) >= 0) s[k++] = m;
          r = add_cmplx(tree, s, k, ta_wgt(t));
          if (r < 0) break;           /* add the reduced transaction */
        }                             /* to the frequent pattern tree */
        if (r >= 0) {                 /* if freq. pattern tree was built */
        r = rec_cmplx(fpg, tree);   /* find freq. item sets recursively */
        if (r >= 0) r = isr_report(fpg->report);
        }                             /* report the empty item set */
        if (fpg->fim16)               /* if a 16-items machine was used, */
        m16_delete(fpg->fim16);     /* delete the 16-items machine */
        ms_delete(tree->mem);         /* delete the memory mgmt. system */
        free(tree); free(fpg->set);   /* and the frequent pattern tree */
        return r;                     /* return the error status */
        }  /* fpg_cmplx() */
        
        /*----------------------------------------------------------------------
        Frequent Pattern Growth (on single tree with simple nodes)
        ----------------------------------------------------------------------*/
        
        static int rec_single (FPGROWTH *fpg, FPTREE *tree, ITEM n)
        {                               /* --- search for frequent item sets */
        int    r;                     /* error status */
        ITEM   i, k, m;               /* loop variables */
        SUPP   pex;                   /* minimum support for perfect exts. */
        FPHEAD *h;                    /* header for current item */
        FPNODE *node, *anc;           /* to traverse the tree nodes */
        
        assert(fpg && tree);          /* check the function arguments */
        i = (tree->fim16) ? 1 : 0;    /* skip packed items if they exist */
        for (r = 0; i < n; i++) {     /* traverse the (other) items, */
        h = tree->heads +i;         /* but skip infrequent items */
        if (h->supp < fpg->supp) continue;
        r = isr_add(fpg->report, h->item, h->supp);
        if (r <  0) break;          /* add current item to the reporter */
        if (r <= 0) continue;       /* check if item needs processing */
        node = h->list;             /* get (first node of) item list */
        if (!node->succ) {          /* if projection would be a chain */
        for (anc = node->parent; anc->id > TA_END; anc = anc->parent) {
          k = anc->id;            /* traverse the list of ancestors */
        if (k >= 0) isr_addpex  (fpg->report, tree->heads[k].item);
        else        isr_addpexpk(fpg->report, k);
        } }                       /* add items as perfect extensions */
        else if ((i > 0)            /* if another item can be added */
        &&       isr_xable(fpg->report, 1)) {
          for (k = 0; k < i; k++) { /* clear item heads and support */
        h = tree->heads +k; h->supp = 0; h->list = NULL; }
          for ( ; node; node = node->succ) {
            for (anc = node->parent; anc->id > TA_END; anc = anc->parent) {
              if (anc->id < 0)      /* traverse the list of ancestors */
        m16_add(tree->fim16, (BITTA)(anc->id & ~TA_END),node->supp);
              else {                /* add packed items to 16-items mach. */
        h = tree->heads +anc->id;   /* traverse the item list */
        if (h->list == anc) break;  /* and the paths to the root */
        h->supp  += anc->supp = node->supp;
        anc->succ = h->list;/* store and sum the node support */
        h->list   = anc;    /* and insert the current ancestor */
              }                     /* into the corresp. item list */
            }
            for ( ; anc->id > TA_END; anc = anc->parent) {
              if (anc->id < 0)      /* traverse the rest of the list */
        m16_add(tree->fim16, (BITTA)(anc->id & ~TA_END),node->supp);
              else {                /* add packed items to 16-items mach. */
        tree->heads[anc->id].supp += node->supp;
                anc->supp += node->supp;
              }                     /* update the support values */
            }                       /* on the rest of the path */
          }
          pex = (fpg->mode & FPG_PERFECT) ? tree->heads[i].supp : SUPP_MAX;
          k = (tree->fim16) ? 1 : 0;/* skip packed items if they exist */
        for (m = 0; k < i; k++) { /* traverse the (other) items again, */
        h = tree->heads +k;     /* but skip infrequent items */
        if (h->supp < fpg->supp)   continue;
        if (h->supp < pex) { m++; continue; }
        h->supp = 0;            /* count the frequent items */
        isr_addpex(fpg->report, h->item);
        }                         /* collect the perfect extensions */
        if (tree->fim16) {        /* if there is a 16-items machine */
        r = m16_mine(tree->fim16);
          if (r < 0) { m = 0; break; }
        }                         /* mine frequent item sets */
        if (m > 0) r = rec_single(fpg, tree, i);
        if (r < 0) break;         /* if the projection is not empty, */
        }                           /* process it recursively */
        r = isr_report(fpg->report);/* report the current item set */
        if (r < 0) break;           /* and check for an error */
        isr_remove(fpg->report, 1); /* remove the current item */
        }                             /* from the item set reporter */
        return r;                     /* return the error status */
        }  /* rec_single() */
        
        /*--------------------------------------------------------------------*/
        
        int fpg_single (FPGROWTH *fpg)
        {                               /* --- search for frequent item sets */
        int        r = 0;             /* result of recursion/functions */
        ITEM       i, k, m;           /* loop variable, number of items */
        TID        j, n;              /* loop variable, number of trans. */
        SUPP       pex;               /* minimum support for perfect exts. */
        ITEM       *s, *d;            /* to build the item maps */
        const ITEM *p;                /* to traverse transaction items */
        const SUPP *f;                /* item frequencies in trans. bag */
        TRACT      *t;                /* to traverse the transactions */
        FPTREE     *tree;             /* created frequent pattern tree */
        FPHEAD     *h;                /* to traverse the item heads */
        
        assert(fpg);                  /* check the function arguments */
        pex = tbg_wgt(fpg->tabag);    /* check against the minimum support */
        if (fpg->supp > pex) return 0;/* and get minimum for perfect exts. */
        if (!(fpg->mode & FPG_PERFECT)) pex = SUPP_MAX;
        n = tbg_cnt(fpg->tabag);      /* get the number of transactions */
        k = tbg_itemcnt(fpg->tabag);  /* and check the number of items */
        if (k <= 0) return isr_report(fpg->report);
        f = tbg_ifrqs(fpg->tabag, 0); /* get the item frequencies */
        if (!f) return -1;            /* in the transaction bag */
        s = fpg->set = (ITEM*)malloc((size_t)(k+k) *sizeof(ITEM));
        if (!s) return -1;            /* create item and support arrays */
        fpg->map = d = s+k;           /* note item map and set buffer */
        if (!(fpg->mode & FPG_FIM16)) m = 0;   /* if 16-items machine, */
        else {      d[0] = s[0] = 0;  m = 1; } /* always keep packed items */
        for (i = m; i < k; i++) {     /* build the item identifier map */
        if (f[i] <  fpg->supp) { d[i] = -1; continue; }
        if (f[i] >= pex)       { isr_addpex(fpg->report, i);
          d[i] = -1; continue; }
        d[i] = m; s[m++] = i;       /* eliminate infrequent items and */
        }                             /* collect perfect extension items */
        if (m <= 0) {                 /* check whether there are items left */
        r = isr_report(fpg->report); free(fpg->set); return r; }
        fpg->dir = +1;                /* only upward item loops possible */
        tree = (FPTREE*)malloc(sizeof(FPTREE) +(size_t)(m-1) *sizeof(FPHEAD));
        if (!tree) { free(fpg->set); return -1; }
        tree->cnt = k = m;            /* allocate the base tree structure */
        tree->dir = fpg->dir;         /* and initialize its fields */
        tree->mem = ms_create(sizeof(FPNODE), 65535);
        if (!tree->mem) { free(tree); free(fpg->set); return -1; }
        tree->root.id   = TA_END;     /* create memory system for the nodes */
        tree->root.supp = 0;          /* and initialize the root node */
        tree->root.succ = tree->root.parent = NULL;
        tree->fim16 = NULL;           /* default: no 16-items machine */
        if (fpg->mode & FPG_FIM16) {  /* if to use a 16-items machine */
        tree->fim16 = m16_create(fpg->dir, fpg->supp, fpg->report);
          if (!tree->fim16) { ms_delete(tree->mem);
            free(tree); free(fpg->set); return -1; }
        }                             /* create a 16-items machine */
        for (i = 0; i < k; i++) {     /* initialize the item heads */
        h = tree->heads+i; h->supp = f[h->item = s[i]]; h->list = NULL; }
        for (j = n; --j >= 0; ) {     /* traverse the transactions and */
        t = tbg_tract(fpg->tabag,j);/* collect the non-eliminated items */
        for (k = 0, p = ta_items(t); *p > TA_END; p++) {
          if      ((m = *p)   <  0) s[k++] = m;
          else if ((m = d[m]) >= 0) s[k++] = m;
        }                           /* add packed items to 16-items mach. */
        r = add_smp16(tree, s, k, ta_wgt(t));
        if (r < 0) break;           /* add the reduced transaction */
        }                             /* to the frequent pattern tree */
        if ((r >= 0) && tree->fim16)  /* if there is a 16-items machine, */
        r = m16_mine(tree->fim16);  /* mine frequent item sets with it */
        if (r >= 0) {                 /* if freq. pattern tree was built */
        r = rec_single(fpg, tree, tree->cnt);
          if (r >= 0) r = isr_report(fpg->report);
        }                             /* find freq. item sets recursively */
        if (tree->fim16)              /* if a 16-items machine was used, */
        m16_delete(tree->fim16);    /* delete the 16-items machine */
        ms_delete(tree->mem);         /* delete the memory mgmt. system */
        free(tree); free(fpg->set);   /* and the frequent pattern tree */
        return r;                     /* return the error status */
        }  /* fpg_single() */
        
        /*----------------------------------------------------------------------
        Frequent Pattern Growth (top-down processing)
        ----------------------------------------------------------------------*/
        
        static int add_topdn (TDTREE *tree, const ITEM *ids, ITEM n, SUPP supp)
        {                               /* --- add an item set to the tree */
        ITEM   i;                     /* buffer for an item */
        TDNODE **p;                   /* insertion position */
        TDNODE *node;                 /* to insert new nodes */
        
        assert(tree                   /* check the function arguments */
        &&    (ids || (n <= 0)) && (supp >= 0));
        p = &tree->root;              /* start the search at the root node */
        while (1) {                   /* traverse the items of the set */
        if (--n < 0) return 0;      /* if all items are processed, abort */
        i = ids[n];                 /* get the next item in the set */
        while (*p && ((*p)->id > i)) p = &(*p)->sibling;
        node = *p;                  /* find the item/insertion position */
        if (!node || (node->id != i)) break;
        node->supp += supp;         /* if the item does not exist, abort */
        p = &node->children;        /* else update the item set support */
        }                             /* and get the list of children */
        node = (TDNODE*)ms_alloc(tree->mem);
        if (!node) return -1;         /* create a new prefix tree node */
        node->id      = i;            /* store the current item and */
        node->supp    = supp;         /* the support of the item set */
        node->sibling = *p;           /* insert the created node */
        *p = node;                    /* into the sibling list */
        while (--n >= 0) {            /* traverse the rest of the items */
        node = node->children = (TDNODE*)ms_alloc(tree->mem);
          if (!node) return -1;       /* create a new prefix tree node */
        node->id      = ids[n];     /* create a new prefix tree node */
        node->supp    = supp;       /* store item and its support */
        node->sibling = NULL;       /* there are no siblings yet */
        }
        node->children = NULL;        /* last created node is a leaf */
        return 1;                     /* return that nodes were added */
        }  /* add_topdn() */
        
        /*--------------------------------------------------------------------*/
        
        static void getsupp2 (TDNODE *node, SUPP *supp)
        {                               /* --- determine conditional support */
        for ( ; node; node = node->sibling) {
          supp[node->id] += node->supp;
          getsupp2(node->children, supp);
        }                             /* recursively sum support per item */
        }  /* getsupp2() */
        
        /*--------------------------------------------------------------------*/
        
        static TDNODE* merge (TDNODE *s1, TDNODE *s2)
        {                               /* --- merge two node lists */
        TDNODE *out, **end = &out;    /* output node list and end pointer */
        
        if (!s1) return s2;           /* if there is only one node list, */
        if (!s2) return s1;           /* simply return the other list */
        end = &out;                   /* start the output list */
        while (1) {                   /* node list merge loop */
        if      (s1->id > s2->id) { /* copy node with singular item */
        *end = s1; end = &s1->sibling; s1 = *end; if (!s1) break; }
        else if (s2->id > s1->id) { /* copy node with singular item */
        *end = s2; end = &s2->sibling; s2 = *end; if (!s2) break; }
        else {                      /* if item occurs in both trees */
        s1->children = merge(s1->children, s2->children);
          s1->supp += s2->supp;     /* merge the children recursively */
        *end = s1; end = &s1->sibling; s1 = *end; s2 = s2->sibling;
        if (!s1 || !s2) break;    /* move node from the first source */
        }                           /* to the output and delete the node */
        }                             /* from the second source */
        *end = (s1) ? s1 : s2;        /* append the remaining nodes */
        return out;                   /* return the merged top-down tree */
        }  /* merge() */
        
        /*--------------------------------------------------------------------*/
        
        static TDNODE* copy3 (TDNODE *src, ITEM *map, MEMSYS *mem)
        {                               /* --- copy a top-down tree */
        ITEM   i;                     /* new item identifier */
        TDNODE *node, *dst;           /* created copy of the node list */
        TDNODE **end = &dst;          /* end of the created copy */
        TDNODE *c, *b = NULL;         /* buffer for copied children */
        
        assert(src && map && mem);    /* check the function arguments */
        do {                          /* sibling list copying loop */
        c = src->children;          /* if there are children, copy them */
        if (c && ((c = copy3(c, map, mem)) == COPYERR)) return COPYERR;
        i = map[src->id];           /* get the new item identifier */
        if (i >= 0) {               /* if to copy the current node */
        *end = node = (TDNODE*)ms_alloc(mem);
          if (!node) return COPYERR;/* create a copy of the current node */
        node->id   = i;           /* store the item and the support and */
        node->supp = src->supp;   /* update the conditional support */
        node->children = c;       /* set the (copied) children */
        end = &node->sibling; }   /* get the new end of the output */
        else if (c)                 /* merge copied children to a buffer */
        b = (!b) ? c : merge(b, c);
        src = src->sibling;         /* get the next sibling */
        } while (src);                /* while there is another node */
        *end = NULL;                  /* terminate the copied list */
        return (!b) ? dst : (!dst) ? b : merge(dst, b);
        }  /* copy3() */                 /* merge with buffered copies */
        
        /*--------------------------------------------------------------------*/
        
        static int rec_topdn (FPGROWTH *fpg, TDTREE *tree)
        {                               /* --- find item sets recursively */
        int    r = 0;                 /* error status */
        ITEM   i, k;                  /* loop variables */
        SUPP   pex;                   /* minimum for perfect extensions */
        TDTREE *proj = NULL;          /* created projection */
        TDNODE *node;                 /* to traverse the nodes */
        SUPP   *s;                    /* to compute the item support */
        ITEM   *map;                  /* to build the item map */
        
        assert(fpg && tree);          /* check the function arguments */
        if (fpg->mode & FPG_TAIL) {   /* if to use head union tail pruning */
        for (map = fpg->map, i = 0; i < tree->cnt; i++)
          *--map = tree->items[i];  /* collect the tail items */
        r = isr_tail(fpg->report, map, tree->cnt);
        if (r) return r;            /* if tail needs no processing, */
        }                             /* abort the recursion */
        if ((tree->cnt > 1)           /* if there is more than one item */
        &&  isr_xable(fpg->report,2)){/* and another item can be added */
        proj = (TDTREE*)malloc(sizeof(TDTREE)
                                 +(size_t)(tree->cnt-2) *sizeof(ITEM));
          if (!proj) return -1;       /* create a frequent pattern tree */
        proj->mem = tree->mem;      /* of the maximally possible size */
        if (ms_push(tree->mem) < 0) { free(proj); return -1; }
        }                             /* note the current memory state */
        for (node = tree->root; node; node = tree->root) {
          r = isr_add(fpg->report, tree->items[node->id], node->supp);
          if (r <  0) break;          /* add current item to the reporter */
        if (r <= 0) {               /* check if item needs processing */
        tree->root = merge(node->sibling, node->children); continue; }
        if (proj && node->children){/* if current node has children */
        memset(s = fpg->cis, 0, (size_t)node->id *sizeof(SUPP));
          getsupp2(node->children,s);/* determine the conditional support */
        pex = (fpg->mode & FPG_PERFECT) ? node->supp : SUPP_MAX;
        map = fpg->map;           /* get perfect extension support */
        for (i = k = 0; i < node->id; i++) {
          if (s[i] <  fpg->supp){ /* traverse items and their support */
        map[i] = -1; continue; } /* eliminate infrequent items and */
        if (s[i] >= pex) {         /* collect perfect extension items */
        map[i] = -1; isr_addpex(fpg->report,tree->items[i]);continue;}
        map[i] = k; proj->items[k++] = tree->items[i];
        }                         /* build item identifier maps */
        if (k > 0) {              /* if the projection is not empty, */
        proj->cnt  = k;         /* note the number of items */
        proj->root = copy3(node->children, map, proj->mem);
        if (proj->root == COPYERR) { r = -1; break; }
        r = rec_topdn(fpg, proj);
        if (r < 0) break;       /* copy the subtree for the item */
        }                         /* and process it recursively */
        }
        r = isr_report(fpg->report);/* report the current item set */
        if (r < 0) break;           /* and check for an error */
        isr_remove(fpg->report, 1); /* remove the current item */
        tree->root = merge(node->sibling, node->children);
        }                             /* prune the processed item */
        if (proj) {                   /* delete the created projection */
        free(proj); ms_pop(tree->mem); }
        return r;                     /* return the error status */
        }  /* rec_topdn() */
        
        /*--------------------------------------------------------------------*/
        
        int fpg_topdn (FPGROWTH *fpg)
        {                               /* --- search for frequent item sets */
        int        r = 0;             /* result of recursion/functions */
        ITEM       i, k, m;           /* loop variable, number of items */
        TID        j, n;              /* loop variable, number of trans. */
        SUPP       pex;               /* minimum support for perfect exts. */
        TRACT      *t;                /* to traverse the transactions */
        ITEM       *s, *d;            /* to traverse flags / items */
        const ITEM *p;                /* to traverse transaction items */
        const SUPP *f;                /* item frequencies in trans. bag */
        TDTREE     *tree;             /* top-down prefix tree */
        
        assert(fpg);                  /* check the function arguments */
        pex = tbg_wgt(fpg->tabag);    /* check against the minimum support */
        if (fpg->supp > pex) return 0;/* and get minimum for perfect exts. */
        if (!(fpg->mode & FPG_PERFECT)) pex = SUPP_MAX;
        n = tbg_cnt(fpg->tabag);      /* get the number of transactions */
        k = tbg_itemcnt(fpg->tabag);  /* and check the number of items */
        if (k <= 0) return isr_report(fpg->report);
        f = tbg_ifrqs(fpg->tabag, 0); /* get the item frequencies */
        if (!f) return -1;            /* in the transaction bag */
        s = fpg->set = (ITEM*)malloc((size_t)(k+k) *sizeof(ITEM)
                                       +(size_t) k    *sizeof(SUPP));
        if (!s) return -1;            /* create item and support arrays */
        fpg->map = d = s+k;           /* note item set buffer and item map */
        fpg->cis = (SUPP*)(d+k);      /* and the item support array */
        for (i = m = 0; i < k; i++) { /* build the item identifier map */
        if (f[i] <  fpg->supp) { d[i] = -1; continue; }
        if (f[i] >= pex)       { isr_addpex(fpg->report, i);
          d[i] = -1; continue; }
        d[i] = m; s[m++] = i;       /* eliminate infrequent items and */
        }                             /* collect perfect extension items */
        if (m <= 0) {                 /* check whether there are items left */
        r = isr_report(fpg->report); free(fpg->set); return r; }
        fpg->dir = +1;                /* only upward item loops possible */
        tree = (TDTREE*)malloc(sizeof(TDTREE) +(size_t)(m-1) *sizeof(ITEM));
        if (!tree) { free(fpg->set); return -1; }
        tree->cnt  = k = m;           /* create a top-down prefix tree and */
        tree->root = NULL;            /* the memory system for the nodes */
        tree->mem  = ms_create(sizeof(TDNODE), 65535);
        if (!tree->mem) { free(tree); free(fpg->set); return -1; }
        memcpy(tree->items, d, (size_t)k *sizeof(ITEM));
        for (j = n; --j >= 0; ) {     /* traverse the transactions and */
        t = tbg_tract(fpg->tabag,j);/* collect the non-eliminated items */
        for (k = 0, p = ta_items(t); *p > TA_END; p++)
          if ((m = d[*p]) >= 0) s[k++] = m;
          r = add_topdn(tree, s, k, ta_wgt(t));
          if (r < 0) break;           /* add the reduced transaction */
        }                             /* to the frequent pattern tree */
        if (r >= 0) {                 /* if freq. pattern tree was built, */
        r = rec_topdn(fpg, tree);   /* find freq. item sets recursively */
        if (r >= 0) r = isr_report(fpg->report);
        }                             /* report the empty item set */
        ms_delete(tree->mem);         /* delete the memory mgmt. system */
        free(tree); free(fpg->set);   /* delete the frequent pattern tree */
        return r;                     /* return the error status */
        }  /* fpg_topdn() */
        
        /*----------------------------------------------------------------------
        Frequent Pattern Growth (on single tree, for rules)
        ----------------------------------------------------------------------*/
        
        int rec_tree (FPGROWTH *fpg, FPTREE *tree, ITEM n)
        {                               /* --- search for frequent item sets */
        int    r = 0;                 /* error status */
        ITEM   i, k, m;               /* loop variables */
        FPHEAD *h;                    /* header for current item */
        FPNODE *node, *anc;           /* to traverse the tree nodes */
        
        assert(fpg && tree);          /* check the function arguments */
        if (!ist_xable(fpg->istree, 1))
          return 0;                   /* if no item can be added, abort */
        if (ist_addchn(fpg->istree) != 0)
          return -1;                  /* add children to current node */
        for (i = 1; i < n; i++) {     /* traverse the items, */
        h = tree->heads +i;         /* but skip the infrequent items */
        if (h->supp < fpg->supp) continue;
        if (ist_down(fpg->istree, h->item) < 0)
          continue;                 /* go down in the item set tree */
        for (k = 0; k < i; k++) {   /* clear item heads and support */
        h = tree->heads +k; h->supp = 0; h->list = NULL; }
        for (node = tree->heads[i].list; node; node = node->succ) {
          for (anc = node->parent; anc->id > TA_END; anc = anc->parent) {
            h = tree->heads +anc->id;   /* traverse the item list */
        if (h->list == anc) break;  /* and the paths to the root */
        h->supp  += anc->supp = node->supp;
        anc->succ = h->list;    /* store and sum the node support */
        h->list   = anc;        /* and insert the current ancestor */
          }                         /* into the corresp. item list */
        for ( ; anc->id > TA_END; anc = anc->parent) {
          tree->heads[anc->id].supp += node->supp;
          anc->supp += node->supp;
        }                         /* update the support values */
        }                           /* on the rest of the path */
        for (k = m = 0; k < i; k++) {
          h = tree->heads +k;       /* traverse the items */
        if (h->supp < fpg->supp) continue;
        ist_setsupp(fpg->istree, h->item, h->supp);
        m += 1;                   /* set the item set support and */
        }                           /* count the frequent items */
        if (m > 0) r = rec_tree(fpg, tree, i);
        if (r < 0) break;           /* if the projection is not empty, */
        ist_up(fpg->istree);        /* process it recursively, */
        }                             /* then go back up in the tree */
        return r;                     /* return the error status */
        }  /* rec_tree() */
        
        /*--------------------------------------------------------------------*/
        
        int fpg_tree (FPGROWTH *fpg)
        {                               /* --- search for frequent item sets */
        int        r = 0;             /* result of recursion/functions */
        ITEM       i, k, m;           /* loop variable, number of items */
        TID        j, n;              /* loop variable, number of trans. */
        ITEM       *s, *d;            /* to build the item maps */
        const ITEM *p;                /* to traverse transaction items */
        const SUPP *f;                /* item frequencies in trans. bag */
        TRACT      *t;                /* to traverse the transactions */
        FPTREE     *tree;             /* created frequent pattern tree */
        FPHEAD     *h;                /* to traverse the item heads */
        
        assert(fpg);                  /* check the function arguments */
        k = tbg_itemcnt(fpg->tabag);  /* get the number of (frequent) items */
        if (k <= 0) return 0;         /* if there are none, abort */
        f = tbg_ifrqs(fpg->tabag, 0); /* get the item frequencies */
        if (!f) return -1;            /* in the transaction bag */
        s = fpg->set = (ITEM*)malloc((size_t)(k+k) *sizeof(ITEM));
        if (!s) return -1;            /* create item and support arrays */
        fpg->map = d = s+k;           /* note item map and set buffer */
        for (i = m = 0; i < k; i++) { /* build the item identifier map */
        if (f[i] < fpg->supp) { d[i] = -1; }
        else                  { d[i] =  m; s[m++] = i; }
        }                             /* collect the frequent items */
        if (m <= 0) { free(fpg->set); return r; }
        tree = (FPTREE*)malloc(sizeof(FPTREE) +(size_t)(m-1) *sizeof(FPHEAD));
        if (!tree)  { free(fpg->set); return -1; }
        tree->cnt = k = m;            /* allocate the base tree structure */
        tree->dir = fpg->dir;         /* and initialize its fields */
        tree->mem = ms_create(sizeof(FPNODE), 65535);
        if (!tree->mem) { free(tree); free(fpg->set); return -1; }
        tree->root.id   = TA_END;     /* create memory system for the nodes */
        tree->root.supp = 0;          /* and initialize the root node */
        tree->root.succ = tree->root.parent = NULL;
        for (i = 0; i < k; i++) {     /* initialize the item heads */
        h = tree->heads+i; h->supp = f[h->item = s[i]]; h->list = NULL; }
        n = tbg_cnt(fpg->tabag);      /* get the number of transactions */
        for (j = n; --j >= 0; ) {     /* traverse the transactions and */
        t = tbg_tract(fpg->tabag,j);/* collect the non-eliminated items */
        for (k = 0, p = ta_items(t); *p > TA_END; p++) {
          if      ((m = *p)   <  0) s[k++] = m;
          else if ((m = d[m]) >= 0) s[k++] = m;
        }                           /* add packed items to 16-items mach. */
        r = add_simple(tree, s, k, ta_wgt(t));
        if (r < 0) break;           /* add the reduced transaction */
        }                             /* to the frequent pattern tree */
        if (r >= 0)                   /* find freq. item sets recursively */
        r = rec_tree(fpg, tree, tree->cnt);
        ms_delete(tree->mem);         /* delete the memory mgmt. system */
        free(tree); free(fpg->set);   /* and the frequent pattern tree */
        return r;                     /* return the error status */
        }  /* fpg_tree() */
        
        /*----------------------------------------------------------------------
        Frequent Pattern Growth (generic)
        ----------------------------------------------------------------------*/
        
        static FPGFN* fpgvars[] = {     /* --- table of fp-growth variants */
        fpg_simple,                   /* simple  nodes (parent/successor) */
        fpg_cmplx,                    /* complex nodes (children/sibling) */
        fpg_single,                   /* top-down processing w/ single tree */
        fpg_topdn,                    /* top-down processing of the tree */
        };
        
        static int cleanup_fpg (FPGROWTH *fpg)
        {                               /* --- clean up on error */
        if (fpg->mode & FPG_NOCLEAN)  /* if not to clean up memory, */
        return E_NOMEM;             /* simply abort the function */
        if (fpg->istree) {            /* free item set tree (for counting) */
        ist_delete(fpg->istree); fpg->istree = NULL; }
        return E_NOMEM;               /* return an error indicator */
        }  /* cleanup_fpg() */
        
        /*--------------------------------------------------------------------*/
        
        void fpg_delete (FPGROWTH *fpg, int deldar)
        {                               /* --- delete an fpgrowth miner */
        cleanup_fpg(fpg);                 /* clean up temporary data */
        if (deldar) {                 /* if to delete data and reporter */
        if (fpg->report) isr_delete(fpg->report, 0);
        if (fpg->tabag)  tbg_delete(fpg->tabag,  1);
        }                             /* delete if existing */
        free(fpg);                    /* delete the base structure */
        }  /* fpg_delete() */
        
        int fpg_report (FPGROWTH *fpg, ISREPORT *report)
        {                               /* --- prepare reporter for fpgrowth */
        TID    n;                     /* number of transactions */
        SUPP   w;                     /* total transaction weight */
        double smax;                  /* absolute maximum support */
        int    mrep;                  /* mode for item set reporter */
        int    e;                     /* evaluation without flags */
        
        assert(fpg && report);        /* check the function arguments */
        fpg->report = report;         /* note the item set reporter */
        
        /* --- make parameters consistent --- */
        mrep = 0;                     /* initialize reporting mode */
        if ((fpg->target & ISR_GENERAS) && (fpg->mode & FPG_REORDER))
          mrep |= ISR_SORT;           /* reordering requires set sorting */
        e = fpg->eval & ~FPG_INVBXS;
        if ((fpg->target & ISR_RULES) /* remove flags from measure code */
        || ((e > RE_NONE) && (e < RE_FNCNT)))
          mrep |= ISR_NOFILTER;       /* no filtering if done in fpgrowth */
        
        /* --- configure item set reporter --- */
        w = tbg_wgt(fpg->tabag);      /* set support and size range */
        smax = (fpg->smax < 0) ? -fpg->smax
          : (fpg->smax/100.0) *(double)w *(1-DBL_EPSILON);
        isr_setsupp(report, (RSUPP)fpg->supp, (RSUPP)floorsupp(smax));
        isr_setsize(report, fpg->zmin, fpg->zmax);
        if (e == FPG_LDRATIO)         /* set additional evaluation measure */
        isr_seteval(report, isr_logrto, NULL, +1, fpg->thresh);
        n = (fpg->mode & FPG_PREFMT)  /* get range of numbers to preformat */
        ? (TID)ib_maxfrq(tbg_base(fpg->tabag)) : -1;
        if ((isr_prefmt(report, (TID)fpg->supp, n)      != 0)
              ||  (isr_settarg(report, fpg->target, mrep, -1) != 0))
          return E_NOMEM;             /* set pre-format and target type */
        return 0;                     /* return 'ok' */
        }  /* fpg_report() */
        
        /*--------------------------------------------------------------------*/
        
        
        
        typedef struct {                /* --- item set report data --- */
        SEXP   res;                   /* constructed result object */
        size_t size;                  /* size     of result array */
        size_t cnt;                   /* elements in result array */
        int    istr;                  /* item type (integer or string) */
        int    len;                   /* number     of values to report */
        CCHAR  *rep;                  /* indicators of values to report */
        int    err;                   /* error flag */
        } REPDATA;                      /* (item set report data) */
        
        /*--------------------------------------------------------------------*/
        
        static void isr_iset2RObj (ISREPORT *rep, void *data)
        {                               /* --- report an item set */
        REPDATA *rd = data;           /* type the data pointer */
        size_t  k, n;                 /* size of result buffer, loop var. */
        ITEM    i, m;                 /* loop variable for items */
        int     v;                    /* loop variable for info. values */
        SEXP    p;                    /* resized result buffer, item buffer */
        SEXP    rset;                 /* new R object for item set */
        int     *iset;                /* item set elements (if integer) */
        SEXP    info;                 /* information for item set */
        double  *r;                   /* real array for information */
        RSUPP   supp, base;           /* item set support and base support */
        SEXP    relt;                 /* result element */
        
        assert(rep && data);          /* check the function arguments */
        if (rd->err) return;          /* if there was an error, do nothing */
        n = rd->size;                 /* get the current array size */
        if (rd->cnt >= n) {           /* if the result array is full */
        n += (n > BLKSIZE) ? n >> 1 : BLKSIZE;
          p = PROTECT(allocVector(VECSXP, (R_xlen_t)n));
          if (rd->res) {              /* if there is an old vector/list */
        for (k = 0; k < rd->cnt; k++)
          SET_VECTOR_ELT(p, (R_xlen_t)k, VECTOR_ELT(rd->res,(R_xlen_t)k));
            UNPROTECT(2); PROTECT(p); /* copy the existing item sets and */
          }                           /* transfer protection to new array */
        rd->res = p; rd->size = n;  /* set the (new) array/list */
        }                             /* and the new array size */
        m = isr_cnt(rep);             /* get the number of items */
        if (rd->istr) {               /* if items are strings */
        rset = PROTECT(allocVector(STRSXP, (R_xlen_t)m));
          for (i = 0; i < m; i++) {   /* create object for item set */
        p = mkChar(isr_basename(rep, isr_itemx(rep, i)));
            SET_STRING_ELT(rset,i,p); /* map identifiers to items and */
          } }                         /* store items in string array */
        else {                        /* if items are integer numbers */
        rset = PROTECT(allocVector(INTSXP, (R_xlen_t)m));
          iset = INTEGER(rset);       /* create object for item set */
        for (i = 0; i < m; i++)     /* map identifiers to items */
        iset[i] = (int)(ptrdiff_t)isr_itemobj(rep, isr_itemx(rep, i));
        }                             /* store items in integer array */
        info = PROTECT(allocVector(REALSXP, (R_xlen_t)rd->len));
        r    = REAL(info);            /* create an information array */
        supp = isr_supp(rep);         /* get the item set support */
        base = isr_suppx(rep, 0);     /* and the total transaction weight */
        for (v = 0; v < rd->len; v++){/* traverse the values to store */
        switch (rd->rep[v]) {       /* evaluate the value indicator */
        case 'a': r[v] = (double)supp;                    break;
        case 's': r[v] = (double)supp /(double)base;      break;
        case 'S': r[v] = (double)supp /(double)base *100; break;
        case 'p': r[v] = isr_eval(rep);                   break;
        case 'P': r[v] = isr_eval(rep) *100;              break;
        case 'e': r[v] = isr_eval(rep);                   break;
        case 'E': r[v] = isr_eval(rep) *100;              break;
        case 'Q': r[v] = (double)base;                    break;
        default : r[v] = 0.0;                             break;
        }                           /* get the requested value and */
        }                             /* store it in the information array */
        relt = PROTECT(allocVector(VECSXP, 2));
        SET_VECTOR_ELT(relt,0,rset);  /* build element for each item set */
        SET_VECTOR_ELT(relt,1,info);  /* and store set and information */
        SET_VECTOR_ELT(rd->res, (R_xlen_t)rd->cnt, relt);
        rd->cnt += 1;                 /* store and count created item set */
        UNPROTECT(3);                 /* release the sub-objects */
        }  /* isr_iset2RObj() */
        
        /*--------------------------------------------------------------------*/
        
        static double lift (RSUPP supp, RSUPP body, RSUPP head, RSUPP base)
        {                               /* --- compute lift value of a rule */
        return ((body <= 0) || (head <= 0)) ? 0
          : ((double)supp*(double)base) /((double)body*(double)head);
        }  /* lift() */
        
        /*--------------------------------------------------------------------*/
        
        static void isr_rule2RObj (ISREPORT *rep, void *data,
                                   ITEM item, RSUPP body, RSUPP head)
        {                               /* --- report an association rule */
        REPDATA *rd = data;           /* type the data pointer */
        ITEM    i, m, o, z;           /* loop variable, array size */
        int     v;                    /* loop variable, index offset */
        size_t  k, n;                 /* size of result buffer */
        SEXP    p;                    /* resized result array */
        SEXP    cons;                 /* new R object for rule head */
        SEXP    ante;                 /* new R object for rule body */
        int     *iset;                /* item set elements (if integer) */
        SEXP    info;                 /* information for item set */
        double  *r;                   /* real array for information */
        RSUPP   supp, base;           /* item set support and base support */
        SEXP    relt;                 /* result element */
        
        assert(rep && data            /* check the function arguments */
        &&    (body > 0) && (head > 0));
        assert(isr_uses(rep, item));  /* head item must be in item set */
        if (rd->err) return;          /* if there was an error, do nothing */
        n = rd->size;                 /* get the current array size */
        if (rd->cnt >= n) {           /* if the result array is full */
        n += (n > BLKSIZE) ? n >> 1 : BLKSIZE;
          p = PROTECT(allocVector(VECSXP, (R_xlen_t)n));
          if (rd->res) {              /* if there is an old vector/list */
        for (k = 0; k < rd->cnt; k++)
          SET_VECTOR_ELT(p, (R_xlen_t)k, VECTOR_ELT(rd->res,(R_xlen_t)k));
            UNPROTECT(2); PROTECT(p); /* copy the existing item sets and */
          }                           /* transfer protection to new array */
        rd->res = p; rd->size = n;  /* set (new) array/list and its size */
        }
        m = isr_cnt(rep);             /* get the number or items */
        if (rd->istr) {               /* if items are strings */
        cons = PROTECT(allocVector(STRSXP, 1));
          SET_STRING_ELT(cons, 0, mkChar(isr_basename(rep, item)));
          ante = PROTECT(allocVector(STRSXP, (R_xlen_t)(m-1)));
          for (i = o = 0; i < m; i++){/* map identifiers to items */
        z = isr_itemx(rep, i);    /* get the next item and skip it */
        if (z == item) continue;  /* if it is the head of the rule */
        SET_STRING_ELT(ante, o, mkChar(isr_basename(rep, z))); o++;
          } }                         /* store items in string array */
        else {                        /* if items are integer numbers */
        cons = PROTECT(allocVector(INTSXP, 1));
          INTEGER(cons)[0] = (int)(ptrdiff_t)isr_itemobj(rep, item);
          ante = PROTECT(allocVector(INTSXP, (R_xlen_t)(m-1)));
          iset = INTEGER(ante);         /* create object for antecedent */
        for (i = o = 0; i < m; i++) { /* map identifiers to items */
        z = isr_itemx(rep, i);      /* get the next item and skip it */
        if (z == item) continue;    /* if it is the head of the rule */
        iset[o++] = (int)(ptrdiff_t)isr_itemobj(rep, z);
        }                             /* store items in integer array */
        }
        info = PROTECT(allocVector(REALSXP, (R_xlen_t)rd->len));
        r    = REAL(info);            /* create an information array */
        supp = isr_supp(rep);         /* get the item set support */
        base = isr_suppx(rep, 0);     /* and the total transaction weight */
        for (v = 0; v < rd->len; v++){/* traverse the values to store */
        switch (rd->rep[v]) {       /* evaluate the value indicator */
        case 'a': r[v] = (double)supp;                      break;
        case 'b': r[v] = (double)body;                      break;
        case 'h': r[v] = (double)head;                      break;
        case 's': r[v] = (double)supp /(double)base;        break;
        case 'S': r[v] = (double)supp /(double)base *100;   break;
        case 'x': r[v] = (double)body /(double)base;        break;
        case 'X': r[v] = (double)body /(double)base *100;   break;
        case 'y': r[v] = (double)head /(double)base;        break;
        case 'Y': r[v] = (double)head /(double)base *100;   break;
        case 'c': r[v] = (double)supp /(double)body;        break;
        case 'C': r[v] = (double)supp /(double)body *100;   break;
        case 'l': r[v] = lift(supp, body, head, base);      break;
        case 'L': r[v] = lift(supp, body, head, base) *100; break;
        case 'e': r[v] = isr_eval(rep);                     break;
        case 'E': r[v] = isr_eval(rep) *100;                break;
        case 'Q': r[v] = (double)base;                      break;
        default : r[v] = 0.0;                               break;
        }                           /* get the requested value and */
        }                             /* store it in the information array */
        relt = PROTECT(allocVector(VECSXP, 3));
        SET_VECTOR_ELT(relt,0,cons);  /* build element for rule */
        SET_VECTOR_ELT(relt,1,ante);  /* and store head and body */
        SET_VECTOR_ELT(relt,2,info);  /* and rule information */
        SET_VECTOR_ELT(rd->res, (R_xlen_t)rd->cnt, relt);
        rd->cnt += 1;                 /* store and count created ass. rule */
        UNPROTECT(4);                 /* release the sub-objects */
        }  /* isr_rule2RObj() */
        
        /*--------------------------------------------------------------------*/
        
        static SEXP psp_toRObj (PATSPEC *psp, double scale, int format)
        {                               /* --- report pattern spectrum */
        ITEM   z;                     /* loop variable for sizes */
        SUPP   s;                     /* loop variable for support values */
        size_t i, n;                  /* list size, result list index */
        size_t k;                     /* number of occurrences */
        double *r;                    /* to store the values */
        SEXP   res;                   /* created R list object */
        SEXP   row;                   /* R object  for row-wise    rep. */
        SEXP   col[3];                /* R objects for column-wise rep. */
        int    *sizes;                /* pattern sizes */
        int    *supps;                /* support values */
        double *freqs;                /* occurrence frequencies */
        
        assert(psp);                  /* check the function arguments */
        n = psp_sigcnt(psp);          /* get the number of signatures */
        if ((format == '=')           /* if list/vector of triplets */
        ||  (format == '-')) {        /* (size, support, frequency) */
        res = PROTECT(allocVector(VECSXP, (R_xlen_t)n));
          for (i = 0, z = psp_min(psp); z <= psp_max(psp); z++) {
            for (s = psp_min4sz(psp, z); s <= psp_max4sz(psp, z); s++) {
              k = psp_getfrq(psp,z,s);/* traverse the pattern signatures */
        if (k <= 0) continue;   /* skip empty counters */
        row  = PROTECT(allocVector(REALSXP, 3));
        r    = REAL(row);       /* create a row with three elements */
        r[0] = (double)z;       /* store the patter size, */
        r[1] = (double)s;       /* the pattern support, */
        r[2] = (double)k *scale;/* and the frequency */
        SET_VECTOR_ELT(res, (R_xlen_t)i, row); i += 1;
        UNPROTECT(1);           /* set the row in the output vector */
            }                         /* and count and unprotect the row */
          } }                         /* (protected as part of 'res') */
        else {                        /* if three arrays (size,supp,freq) */
        res    = PROTECT(allocVector(VECSXP,  (R_xlen_t)3));
          col[0] = PROTECT(allocVector(INTSXP,  (R_xlen_t)n));
          SET_VECTOR_ELT(res, 0, col[0]); UNPROTECT(1);
          col[1] = PROTECT(allocVector(INTSXP,  (R_xlen_t)n));
          SET_VECTOR_ELT(res, 1, col[1]); UNPROTECT(1);
          col[2] = PROTECT(allocVector(REALSXP, (R_xlen_t)n));
          SET_VECTOR_ELT(res, 2, col[2]); UNPROTECT(1);
          sizes = INTEGER(col[0]);    /* create two integer and */
        supps = INTEGER(col[1]);    /* one real-valued array for */
        freqs = REAL   (col[2]);    /* size, support and frequency */
        for (i = 0, z = psp_min(psp); z <= psp_max(psp); z++) {
          for (s = psp_min4sz(psp, z); s <= psp_max4sz(psp, z); s++) {
            k = psp_getfrq(psp,z,s);/* traverse the pattern signatures */
        if (k <= 0) continue;   /* skip empty counters */
        sizes[i]   = z; supps[i] = s;
        freqs[i++] = (double)k *scale;
          }                         /* store the signature elements */
        }                           /* in the corresponding arrays */
        }
        assert(i == n);               /* check signature counter */
        return res;                   /* return created pattern spectrum */
        }  /* psp_toRObj() */
        
        /*--------------------------------------------------------------------*/
        
        static int repinit (REPDATA *data, ISREPORT *isrep)
        {                               /* --- initialize reporting */
        int target = ISR_SETS;
          CCHAR    *report = "a";
          assert(data && isrep && report); /* check the function arguments */
        data->err = 0;                /* initialize the error indicator */
        if ((report[0] == '#')        /* if to get a pattern spectrum */
        ||  (report[0] == '|')        /* "#", "|": column-wise */
        ||  (report[0] == '=')        /* "=", "-": row-wise */
        ||  (report[0] == '-'))
        return isr_addpsp(isrep, NULL);
        data->res  = NULL;            /* initialize the report structure */
        data->size = data->cnt = 0;   /* and the array parameters */
        data->istr = ((ib_mode(isr_base(isrep)) & IB_OBJNAMES) == 0);
        data->len  = (int)strlen(data->rep = report);
        if (target & ISR_RULES) isr_setrule(isrep, isr_rule2RObj, data);
        else                    isr_setrepo(isrep, isr_iset2RObj, data);
        return 0;                     /* set the report function */
        }  /* repinit() */              /* and return 'ok' */
        
        /*--------------------------------------------------------------------*/
        
        static int repterm (REPDATA *data, ISREPORT *isrep)
        {                               /* --- terminate reporting */
        size_t k;                     /* loop variable */
        SEXP   p;                     /* resized R vector/list */
        CCHAR    *report = "a";
        
        assert(data && isrep && report); /* check the function arguments */
        if (data->err) return data->err -1;
        if ((report[0] == '#')        /* if to get a pattern spectrum */
        ||  (report[0] == '|')        /* "#", "|": column-wise */
        ||  (report[0] == '=')        /* "=", "-": row-wise */
        ||  (report[0] == '-')) {
        data->res = psp_toRObj(isr_getpsp(isrep), 1.0, report[0]);
          return data->err = (data->res) ? 0 : -1;
        }                             /* make R pattern spectrum */
        if (data->cnt != data->size) {/* if result list has wrong size */
        p = PROTECT(allocVector(VECSXP, (R_xlen_t)data->cnt));
          for (k = 0; k < data->cnt; k++) /* create a new result list/array */
        SET_VECTOR_ELT(p, (R_xlen_t)k, VECTOR_ELT(data->res,(R_xlen_t)k));
          UNPROTECT(2); PROTECT(p);   /* copy the existing item sets */
        data->res = p; data->size = data->cnt;
        }                             /* set array/list and its size */
        return data->err;             /* return the error status */
        }  /* repterm() */
        
        static int get_int (SEXP p, int dflt)
        {                               /* --- get an integer parameter */
        assert(p);                    /* check the function argument */
        if (length(p) <  1)       return dflt;
        if (TYPEOF(p) == LGLSXP)  return (LOGICAL(p)[0]) ? 1 : 0;
        if (TYPEOF(p) == INTSXP)  return (int)INTEGER(p)[0];
        if (TYPEOF(p) == REALSXP) return (int)REAL(p)[0];
        return dflt;                  /* extract and convert first element */
        }  /* get_int() */
        
        static double get_dbl (SEXP p, double dflt)
        {                               /* --- get a double parameter */
        assert(p);                    /* check the function argument */
        if (length(p) <  1)       return dflt;
        if (TYPEOF(p) == LGLSXP)  return (LOGICAL(p)[0]) ? 1.0 : 0.0;
        if (TYPEOF(p) == INTSXP)  return (double)INTEGER(p)[0];
        if (TYPEOF(p) == REALSXP) return (double)REAL(p)[0];
        return dflt;                  /* extract and convert first element */
        }  /* get_dbl() */
        
        static TABAG* tbg_fromRObj (SEXP tracts)
        {                               /* --- create a transaction bag */
        int      e = 0;               /* error flag */
        TID      k, m;                /* trans. identifier, loop variable */
        ITEM     i, n;                /* item   identifier, loop variable */
        int      t;                   /* item type (integer or string) */
        SEXP     p;                   /* to traverse the transactions */
        ITEMBASE *ibase;              /* underlying item base */
        TABAG    *tabag;              /* created transaction bag */
        
        assert(tracts);               /* check the function argument */
        t = TYPEOF(VECTOR_ELT(tracts, 0));
        ibase = (t == INTSXP) ? ib_create(IB_OBJNAMES, 0, ST_INTFN, (OBJFN*)0) : ib_create(0, 0);      /* create an item base */
        if (!ibase) return NULL;      /* for integers or strings */
        tabag = tbg_create(ibase);    /* create a transaction bag */
        if (!tabag) { ib_delete(ibase); return NULL; }
        m = length(tracts);           /* get the number of transactions */
        for (k = 0; k < m; k++) {     /* and traverse the transactions */
        ib_clear(ibase);            /* clear the internal transaction */
        p = VECTOR_ELT(tracts, k);  /* get the next R transaction */
        n = (ITEM)length(p);        /* and its length */
        if (t == INTSXP) {          /* if items are integer numbers */
        for (i = 0; i < n; i++) { /* traverse the items (integers) */
        if (ib_add2ta(ibase, INTEGER(p) +i) < 0) { e = -1; break; }      /* add items to internal transaction */
        }
        }                       /* and check for success */
        else {                      /* if items are character strings */
        for (i = 0; i < n; i++) { /* traverse the items (strings) */
        if (ib_add2ta(ibase, CHAR(STRING_ELT(p, i))) < 0) {e = -1; break; }      /* add items to internal transaction */
        }                         /* and check for success */
        }                           /* afterward set transaction weight */
        if (e) break;               /* check for success */
        ib_finta(ibase, 1);
        if (tbg_addib(tabag) < 0) { e = -1; break; }
        }                             /* add the transaction to the bag */
        if (e) { tbg_delete(tabag, 1); return NULL; }
        return tabag;                 /* return the created transaction bag */
        }  /* tbg_fromRObj() */
        
        FPGROWTH* fpg_create (double smin, ITEM zmin, ITEM zmax)
        {                               /* --- create an fpgrowth miner */
        FPGROWTH *fpg;                /* created fpgrowth miner */
        int      e;                   /* evaluation without flags */
        double smax = 100.0;
        int eval = 'x';
        double thresh = 10;
        int mode = FPG_DEFAULT;
        
        /* --- make parameters consistent --- */
        e = eval & ~FPG_INVBXS;       /* get target and remove flags */
        mode &= ~FPG_TAIL;          /* maximal frequent item sets */
        
        /* --- create an fpgrowth miner --- */
        fpg = (FPGROWTH*)malloc(sizeof(FPGROWTH));
        if (!fpg) return NULL;        /* create an fpgrowth miner */
        fpg->target = ISR_FREQUENT;         /* and store all parameters */
        fpg->smin   = smin;
        fpg->smax   = smax;
        fpg->supp   = 1;
        fpg->body   = 1;
        fpg->conf   = 1;
        fpg->zmin   = zmin;
        fpg->zmax   = zmax;
        fpg->eval   = 'x';
        fpg->agg    = 'x';
        fpg->thresh = thresh/100.0;
        fpg->algo   = FPG_SIMPLE;
        fpg->mode   = mode;
        fpg->tabag  = NULL;
        fpg->report = NULL;
        fpg->dir    = 1;
        fpg->set    = NULL;
        fpg->map    = NULL;
        fpg->cis    = NULL;
        fpg->fim16  = NULL;
        fpg->istree = NULL;
        return fpg;                   /* return the created fpgrowth miner */
        }  /* fpg_create() */
        
        int fpg_data (FPGROWTH *fpg, TABAG *tabag, int sort)
        {                               /* --- prepare data for fpgrowth */
        ITEM    m;                    /* number of items */
        ITEM    pack;                 /* number of items to pack */
        double  smin;                 /* absolute minimum support */
        SUPP    w;                    /* total transaction weight */
        int     e;                    /* evaluation without flags */
        clock_t t;                    /* timer for measurements */
        int mode = 0;
        
        assert(fpg && tabag);         /* check the function arguments */
        fpg->tabag = tabag;           /* note the transaction bag */
        
        /* --- compute data-specific parameters --- */
        w = tbg_wgt(tabag);           /* compute absolute minimum support */
        smin = ceilsupp((fpg->smin < 0) ? -fpg->smin
                          : (fpg->smin/100.0) *(double)w *(1-DBL_EPSILON));
        fpg->body = (SUPP)smin;       /* compute body and body&head support */
        if ((fpg->target & ISR_RULES) && !(fpg->mode & FPG_ORIGSUPP))
          smin *= fpg->conf *(1-DBL_EPSILON);
        fpg->supp = (SUPP)ceilsupp(smin);
        
        /* --- make parameters consistent --- */
        pack = fpg->mode & FPG_FIM16; /* get number of items to pack */
        if (pack > 16) pack = 16;     /* pack at most 16 items */
        if (fpg->algo == FPG_COMPLEX) /* for complex fp-trees, if needed, */
        pack = 0;                   /* items are packed in the recursion */
        if (mode & FPG_NOPACK)        /* if excluded by processing mode, */
        pack = 0;                   /* do not pack items */
        
        /* --- sort and recode items --- */
        if (!(mode & FPG_NORECODE)) { /* if to sort and recode the items */
        CLOCK(t);
          /* start timer, */
          if (fpg->mode & FPG_REORDER)/* simplified sorting if reordering */
          sort = (sort < 0) ? -1 : (sort > 0) ? +1 : 0;
          m = tbg_recode(tabag, fpg->supp, -1, -1, -sort);
          if (m < 0) return E_NOMEM;  /* recode items and transactions */
          if (m < 1) return E_NOITEMS;/* and check the number of items */
        }                             /* print a log message */
          
          /* --- sort and reduce transactions --- */
          CLOCK(t);                     /* start timer, print log message */
          e = fpg->eval & ~FPG_INVBXS;  /* filter transactions if possible */
          if (!(mode & FPG_NOFILTER) && !(fpg->target & ISR_RULES)
                &&  ((e <= RE_NONE) || (e >= RE_FNCNT)))
            tbg_filter(tabag, fpg->zmin, NULL, 0);
          if (!(mode & FPG_NOSORT)) {   /* if to sort items and transactions, */
          tbg_itsort(tabag, +1, 0);   /* sort items in transactions and */
          tbg_sort  (tabag, +1, 0);   /* sort the trans. lexicographically */
          if (!(mode & FPG_NOREDUCE)) /* if to combine equal transactions, */
          tbg_reduce(tabag, 0);     /* reduce transactions to unique ones */
          }                             /* (need sorting for reduction) */
          if (pack > 0)                 /* if to use a 16-items machine, */
          tbg_pack(tabag, pack);      /* pack the most frequent items */
          return 0;                     /* return 'ok' */
        }  /* fpg_data() */
          
          ISREPORT* isr_createx (ITEMBASE *base, ITEM max)
          {                               /* --- create an item set reporter */
          ISREPORT   *rep;              /* created item set reporter */
          ITEM       i, k, n;           /* loop variable, number of items */
          double     b, x;              /* logarithms of item probabilities */
          const char *name;             /* to traverse the item names */
          size_t     m;            /* length of an item name */
          
          assert(base);                 /* check the function argument */
          n   = ib_cnt(base);           /* get the number of items and */
          k   = (max > n) ? max : n;    /* the maximum pattern length */
          rep = (ISREPORT*)malloc(sizeof(ISREPORT)
                                    +(size_t)(k+k+1) *sizeof(char*));
          rep->base    = base;          /* note the item base */
          rep->target  = ISR_ALL;       /* report all item sets by default */
          rep->mode    = 0;             /* no special mode set yet */
          rep->zmin    = 1;             /* set the default size range */
          rep->zmax    = ITEM_MAX;
          rep->xmax    = ITEM_MAX;
          rep->size    = k;
          rep->smin    = 1;             /* set the default support range */
          rep->smax    = RSUPP_MAX;
          rep->border  = NULL;          /* clear the filtering border */
          rep->bdrsize = rep->bdrcnt = 0;
          rep->pfx     = rep->cnt = 0;  /* init. the number of items */
          rep->clomax  = NULL;          /* clear the filtering tools */
          rep->gentab  = NULL;          /* (closed/maximal/generators) */
          rep->sto     = RSUPP_MAX;     /* clear the storage flag and */
          rep->dir     = 0;             /* the item sorting direction */
          rep->iset    = NULL;          /* cleat the item set buffer */
          rep->evalfn  = (ISEVALFN*)0;  /* clear add. evaluation function */
          rep->evaldat = NULL;          /* and the corresponding data */
          rep->evaldir = 1;             /* default: threshold is minimum */
          rep->evalthh = rep->eval = 0; /* clear evaluation and its minimum */
          rep->repofn  = (ISREPOFN*)0;  /* clear item set report function */
          rep->repodat = NULL;          /* and the corresponding data */
          rep->rulefn  = (ISRULEFN*)0;  /* clear rule report function */
          rep->ruledat = NULL;          /* and the corresponding data */
          rep->scan    = 0;             /* clear scanable items flag */
          rep->str     = NULL;          /* no user-specified format strings */
          rep->hdr     = "";            /* item set/rule header */
          rep->sep     = " ";           /* item separator */
          rep->imp     = " <- ";        /* implication sign */
          rep->iwf     = ":%w";         /* item weight format */
          rep->info    = " (%a)";       /* item set/rule information format */
          rep->inames  = (const char**)(rep->pos +k+1);
          rep->nmax    = rep->nsum = 0; /* clear maximum/sum of name lengths */
          rep->repcnt  = 0;             /* init. the item set counter */
          rep->psp     = NULL;          /* clear pattern spectrum variable */
          rep->ints    = NULL;          /* clear pre-formatted integers */
          rep->imax    = -1;
          rep->file    = NULL;          /* clear the output file and its name */
          rep->name    = NULL;          /* and the file write buffer */
          rep->buf     = rep->next   = rep->end    = NULL;
          rep->tidfile = NULL;          /* clear transaction id output file */
          rep->tidname = NULL;          /* and its name */
          rep->tidbuf  = rep->tidnxt = rep->tidend = NULL;
          rep->occs    = NULL;
          rep->tids    = NULL;
          rep->tidcnt  = 0;
          rep->tracnt  = 0;
          rep->miscnt  = 0;
          rep->fast    = -1;            /* default: only count the item sets */
          rep->fosize  = 0;
          rep->out     = NULL;          /* there is no output buffer yet */
          rep->pxpp    = (ITEM*)  malloc((size_t)(k+k+k+2) *sizeof(ITEM));
          rep->iset    = (ITEM*)  malloc((size_t)(k+1)     *sizeof(ITEM));
          rep->supps   = (RSUPP*) malloc((size_t)(k+1)     *sizeof(RSUPP));
          rep->wgts    = (double*)calloc((size_t)(k+n+1),   sizeof(double));
          rep->stats   = (size_t*)calloc((size_t)(k+1),     sizeof(size_t));
          if (!rep->pxpp || !rep->iset || !rep->supps || !rep->wgts
                ||  !rep->stats) { isr_delete(rep, 0); return NULL; }
          memset(rep->pxpp, 0, (size_t)(n+1) *sizeof(ITEM));
          rep->pexs    = rep->pxpp +n+1;/* allocate memory for the arrays */
          rep->items   = rep->pexs += k;/* and organize and init. the arrays */
          rep->ldps    = rep->wgts +k+1;
          *rep->wgts   = (double)(*rep->supps = ib_getwgt(base));
          b = (double)rep->supps[0];    /* get the empty set support */
          b = (b > 0) ? log(b) /LN_2 : 0;
          for (i = 0; i < n; i++) {     /* traverse the items */
          x = (double)ib_getfrq(rep->base, i);
            rep->ldps[i] = (x > 0) ? log(x) /LN_2 -b : 0;
          }                             /* compute bin. logs. of item probs. */
          memset((void*)rep->inames, 0, (size_t)(n+1) *sizeof(char*));
          for (rep->nmax = rep->nsum = 0, i = 0; i < n; i++) {
            name = ib_xname(base, i);   /* traverse items and their names */
          if (!rep->scan)             /* if to use items names directly, */
          m = strlen(name);         /* simply get their string lengths */
          rep->nsum += m;             /* sum name size and find maximum */
          if (m > rep->nmax) rep->nmax = m;
          rep->inames[i] = name;      /* store the (formatted) item name */
          if (!name) { isr_delete(rep, 0); return NULL; }
          }                             /* check for proper name copying */
          return rep;                   /* return created item set reporter */
          }  /* isr_createx() */
          
          int fpg_mine (FPGROWTH *fpg, ITEM prune, int order)
          {                               /* --- fpgrowth algorithm */
          int      r;                   /* result of function call */
          int      e;                   /* evaluation without flags */
          clock_t  t;                   /* timer for measurements */
          
          assert(fpg);                  /* check the function arguments */
          
          /* --- make algorithm and modes consistent --- */
          e = fpg->eval & ~FPG_INVBXS;  /* remove flags from measure code */
          if (e <= RE_NONE)             /* if there is no evaluation, */
          prune = ITEM_MIN;           /* do not prune with evaluation */
          
          /* --- find frequent item sets/association rules --- */
          if (!(fpg->target & ISR_RULES)/* if to find plain item sets */
          &&  ((e <= RE_NONE) || (e >= RE_FNCNT))) {
            CLOCK(t);                   /* start the timer for the search */
          r = fpgvars[fpg->algo](fpg);
          if (r < 0) return E_NOMEM;  /* search for frequent item sets */
          }
          return 0;                     /* return 'ok' */
          }  /* fpg_mine() */
          
          
          SEXP f4r_fpgrowth (SEXP ptracts, SEXP psupp, SEXP pzmin, SEXP pzmax)
          {                               /* --- frequent item set mining */
          double   supp    = 10.0;      /* minimum support of an item set */
          int      zmin    =  0;        /* minimum size of an item set */
          int      zmax    = -1;        /* maximum size of an item set */
          long int prune   = LONG_MIN;  /* min. size for evaluation filtering */
          TABAG    *tabag;              /* created transaction bag */
          ISREPORT *isrep;              /* item set reporter */
          FPGROWTH *fpgrowth;           /* fpgrowth miner */
          REPDATA  data;                /* data for item set reporting */
          int      r;                   /* result of function call */
          
          /* --- evaluate function arguments --- */
          supp   = get_dbl(psupp, supp);
          zmin   = get_int(pzmin, zmin);
          zmax   = get_int(pzmax, zmax);/* check the size range */
          if (zmax   < 0)    zmax = ITEM_MAX;
          if (zmin   > ITEM_MAX) zmin = ITEM_MAX;
          if (zmax   > ITEM_MAX) zmax = ITEM_MAX;
          
          /* --- create transaction bag --- */
          sig_install();                /* install the signal handler */
          tabag = tbg_fromRObj(ptracts);
          if (!tabag) ERR_MEM();        /* create & init. transaction bag */
          fpgrowth = fpg_create(supp, (ITEM)zmin, (ITEM)zmax);
          if (!fpgrowth) { tbg_delete(tabag, 1); ERR_MEM(); }
          r = fpg_data(fpgrowth, tabag, +2);
          if (r) fpg_delete(fpgrowth,1);/* prepare data for fpgrowth */
          if (r == -1) ERR_MEM();       /* check for an error and no items */
          if (r <   0) { sig_remove(); return allocVector(VECSXP, 0); }
          
          /* --- create item set reporter --- */
          isrep = isr_createx(tbg_base(tabag), 0);
          if (!isrep                    /* create an item set reporter */
          || (fpg_report(fpgrowth, isrep) != 0)
                ||  (repinit(&data, isrep) != 0)
                ||  (isr_setup(isrep) < 0)) { /* prepare the item set reporter */
          fpg_delete(fpgrowth, 1); ERR_MEM(); }
          
          /* --- frequent item set mining --- */
          if (prune < ITEM_MIN) prune = ITEM_MIN;
          if (prune > ITEM_MAX) prune = ITEM_MAX;
          r = fpg_mine(fpgrowth, (ITEM)prune, 0);
          if (r >= 0) r = repterm(&data, isrep);
          
          /* --- clean up --- */
          fpg_delete(fpgrowth, 1);      /* delete the fpgrowth miner */
          if (data.res) UNPROTECT(1);   /* unprotect the result object */
          if (sig_aborted()) { sig_abort(0); ERR_ABORT(); }
          sig_remove();                 /* remove the signal handler */
          if (r != 0) ERR_MEM();        /* check for an error */
          return (data.res) ? data.res : allocVector(VECSXP, 0);
          }  /* f4r_fpgrowth() */         /* return the created R object */