//libraries 
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <pthread.h>

//global variables
//the matrix
double **arr;

//matrix size
int size;

//the precision the calculations will look for 
double precision;

//the number of threads used
int threads;

//barrier initialised 
pthread_barrier_t barrier;

//array to track if all thread's are under/over the precision at each cycle in the calculation 
int *thread_loops;

/*
Function: create array - creates a 2d matrix of (size x size)
Purpose: create an 2d matrix of (size x size) and assign the required memory dynamically
Argument: size - takes the dimensions of the matrix
Returns: the created matrix to the main function
*/
double** createArray(int size){
    //memory is allocated for the matrix
    double** arr = (double**) malloc(sizeof(double*)*size);
    for (int i=0; i<size; i++){
        arr[i] = malloc(sizeof(double)*size);
    }
    //writes the start vlaues into the matrix
    for (int i=0; i<size; i++){
        for (int j=0; j<size; j++){
            //the upper and left rows are set to 1
            if (i==0){
                arr[i][j] = 1;
            }
            else if (j==0){
                arr[i][j] = 1;
            }
            //other elements are set to 0 
            else{
                arr[i][j] = 0;
            }
        }
    }
    //returns the array 
    return arr;
}
/*
Funciton: par solver (parallel solver)
Purpose: Calculates and reassigns based on the average of the numbers around it  
Arguments: (void*) thread's ID 
*/
void* parSolver(void* id){
    int *t_id = (int*) id; 
    //calculates how many threads are necessary - most times will just be the initiallised value, but if the matrix is small it will be reduced 
    int runningThreads = 0;
    //as outerlines dont require calculations, size-2
    if(size-2>threads){
        runningThreads = threads;
    }
    else{
        runningThreads = size-2;
    }
    //calculates which area in the matrix each thread will be responsible for 
    int workload;
    int extra = (size-2)%runningThreads;
    int base = ((size-2)-extra)/runningThreads;
    int start;
    int end;
    if (*t_id < extra){
        workload = base + 1;
    }
    else{
        workload = base;
    }
    //calculating the start and end in the matrix
    if (*t_id < extra){
        start = *t_id*workload + 1;
        //as I will be using a forloop with < later end I wont exclude the end
        end = start + workload;
    }
    else{
        start = extra*(workload+1) + (*t_id - extra)*workload + 1;
        end = start + workload;
    }
    //dynamically allocating memory space for the new values calculated
    //this is necessary as we cannot rewrite values until we have done a barrier wait
    double **new_values = (double**) malloc(sizeof(double*)*workload);
    for (int i=0; i<workload; i++){
        new_values[i] = malloc(sizeof(double)*size);
    }
    int underprecision = 0; 
    int done = 0;
    //while loop until all value changes meet the precision
    while(done == 0){
        underprecision = 1;
        for(int i=start; i<end; i++){
            for(int j=1; j<size-1; j++){
                //calculating the new value
                new_values[i-start][j] = ((arr[i-1][j] + arr[i+1][j] + arr[i][j-1] + arr[i][j+1])/4);
            }
        }    
        //wait for all calculatonis to finish
        pthread_barrier_wait(&barrier);
        //then start rewriting values into the matrix
        for(int i=start; i<end; i++){
            for(int j=1; j<size-1; j++){
                //calculating the difference 
                double diff = fabs(new_values[i-start][j] - arr[i][j]);
                //rewritng happens here
                arr[i][j] = new_values[i-start][j];
                //checking if precision has been reached 
                if (diff > precision){
                    underprecision = 0;
                }
            }
        }
        //checks if all threads are under the precision
        thread_loops[*t_id] = underprecision;
        pthread_barrier_wait(&barrier);
        done = 1;
        //checks each value in the thread_loopos array
        for (int i=0; i<runningThreads; i++){
            //if all are under precision (1) then it exits the while loop
            if(thread_loops[i]==0){
                done = 0;
            }
        }
    }
    for (int i=0; i<workload; i++){
        free(new_values[i]);
    }
    free(new_values);
    free(id);
    return NULL;   
}

/*
Function: create threads 
Purpose: creates threads based on the size of hte matrix (there will be no more threads than the size) (the -2 is because no calculations are done on the edge rows)
Arguments: none
Returns: 
*/
int createThreads(){
    //dynamically allocates memory space for the threads
    pthread_t *thread_id = (pthread_t*) malloc(sizeof(pthread_t)*threads);
    int runningThreads = 0;
    if(size-2>threads){
        runningThreads = threads;
    }
    else{
        runningThreads = size-2;
    }
    //an array to contain each threads' diff to check if they meet the precision
    thread_loops = (int*) malloc(sizeof(int)*runningThreads);
    //the barrier is initialised here
    pthread_barrier_init(&barrier,NULL,runningThreads);
    for (int t=0; t<threads; t++){
        //stops creating threads if there are more threads than matrix size
        if(t<size){
            int *id = malloc(sizeof(int));
            *id = t;
            pthread_create(&(thread_id[t]), NULL,parSolver, id);
        }
    }
    //join threads
    for(int i=0; i<runningThreads; i++){
        pthread_join((thread_id[i]), NULL);
    }
    //destroy the barrier
    pthread_barrier_destroy(&barrier);
    free(thread_id);
    return 0; 
}

/*
Function: seq solver
Purpose: solves the matrix calculation sequentially, made for comparison and evaluation 
Arguments: none 
*/
void seqSolver(){
    int underprecision = 0;
    int count = 0;
    //dynamically allocate memory space for an array that holds new values temporarily 
    double** tempArr = (double**) malloc (sizeof(double*)*size);
    for(int i=0; i<size; i++){
        tempArr[i] = (double*) malloc(sizeof(double)*size);
    }
    //loops until precision is met 
    while (underprecision == 0){
        underprecision = 1;
        for (int i=1; i<size-1; i++){
            for (int j=1; j<size-1; j++){
                //calculation of new values
                double new = ((arr[i-1][j] + arr[i+1][j] + arr[i][j-1] + arr[i][j+1])/4);
                //calculation of difference between new and old values
                double diff = fabs(new - arr[i][j]);
                //write into the temporary matrix
                tempArr[i][j] = new;
                //if under precision, exit loop
                if (diff > precision){
                    underprecision = 0;
                }
            }
        }
        count++;
        for (int i=1; i<size-1; i++){
            for (int j=1; j<size-1; j++){
                //rewriting new values into original matrix
                arr[i][j] = tempArr[i][j];
            }
        }
    }
    for (int i=0; i<size; i++){
        free(tempArr[i]);
    }
    free(tempArr);
}

/*
Function: Main 
Purpose: assigns variables and calls other functions in the code
Arguments: None
Returns: None
*/
int main(){
    //values assigned here
    size = 5;
    precision = 0.00001;
    threads = 2;
    //create the matrix
    arr = createArray(size);
    //prints the first matrix
    for (int i=0; i<size; i++){
        for (int j=0; j<size; j++){
            printf("%lf ", arr[i][j]);
        }
        printf("\n");
    }
    printf("\n");
    //call sequential solver or parallel solver
    //seqSolver();
    createThreads();
    //print out new matrix
    for (int i=0; i<size; i++){
        for (int j=0; j<size; j++){
            printf("%lf ", arr[i][j]);
        }
        printf("\n");
    }
    for (int i=0; i<size; i++){
        free(arr[i]);
    }
    free(arr);
    free(thread_loops);
    return 0;
}