#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>
#include <math.h>
#include <errno.h>

#define N 300
#define MAXTOKENS 256
#define TOKENLENGTH 20

//Here we created an enum type and numbered the incoming tokens, thanks to this we created an integer array named data and we 																		were able to get the next incoming token type, this worked for us in the syntax control.
enum TOKENTYPES {CTRLVAR, FOR, IN, LPARANTH, RPARANTH, PLUS, MINUS, MULTI, LKOSELI, RKOSELI, LSUSLU, RSUSLU, EQUAL, IKINOKTA, VIRGUL, SCALAR, VECTOR, MATRIX,
    TRANSPOSE, SQRT, CHOOSE, HASHTAG, PRINT, PRINTSEP, ID, DIGIT};   


char arr[N];    //In this array, we read the line and keep the incoming characters one by one.
char space[2] = {' ', '\0'}; //we use an array like this to add spaces

int data[N]; //array that stores enum values ​​of tokens

char tokens [MAXTOKENS][TOKENLENGTH]; //array that stores the tokenized line
char expressions [MAXTOKENS][TOKENLENGTH]; //array that stores the expressions' tokens
char exprarr [MAXTOKENS][TOKENLENGTH];  //array which we push the parse functions

char yuzdes[20] = "%s";  //arrays for file.c printing
char yuzdef[20] = "%f";
char yuzded[20] = "%d";
char satiratlama[20] = "printf(\"\\n\");";
char mod[20] = {'%', '\0'};

int forlinenum;
int linenum = 0;
char str[N] ;
int  cur = 0 ;
int exprarr_num = 0;
int forgiris = 0;
int forcikis = 0;
int forsayisi = 0;
int producedmatrixind = 0;
int error = 0;


//function declarations
int  expr(char *) ;
int  term(char *) ;
int  moreterms(char *) ;
int  factor(char *) ;
int  morefactors(char *) ;
int  is_integer(char *) ;


//function for string reversing
char *strrev(char *str)
{
      char *p1, *p2;

      if (! str || ! *str)
            return str;
      for (p1 = str, p2 = str + strlen(str) - 1; p2 > p1; ++p1, --p2)
      {
            *p1 ^= *p2;
            *p2 ^= *p1;
            *p1 ^= *p2;
      }
      return str;
}

//function for comparing 2 strings
int compare_string(char *first, char *second)
{
    while(*first==*second)
    {
        if ( *first == '\0' || *second == '\0' )
            break;

        first++;
        second++;
    }
    if( *first == '\0' && *second == '\0' )
        return 0;
    else
        return -1;
}

//struct type for matrices and vectors, vectors have e_cols = 1
struct Matrix{
    char* name;         
    double  value[256];
    int e_rows;         // number of entries in rows     == number of columns
    int e_cols;         // number of entries in columns  == number of rows
    int size;
    int defined;
};

// struct type for scalar
struct Scalar{
    char* name;
    char* type;
    int i_value;
    double d_value;
};

//struct type for all variables
struct Variable{
    char* name;
    char* type;
};


struct Matrix matrices[N];
struct Scalar scalars[N];
struct Variable variables[N];

int matrixind = 0;
int scalarind = 0;
int variableind = 0;


// function for int control for print
int is_int(double x){

    int z = (int) x;
    if ( x - z == 0){
        return 1;
    }
    else {
        return 0;
    }
}
// function for double control
int is_double(double x){


    int z = (int) x;
    if ( x - z > 0){
        return 1;
    }
    else {
        return 0;
    }
}

int isNumber(char s[]) {    // if s is integer, it returns 1, if not returns 0
    for (int i = 0; s[i]!= '\0'; i++)
    {
        if (isdigit(s[i]) == 0)
              return 0;
    }
    return 1;
}



int char_doub (char inp[]){          // if inp is double, it returns 1, if not returns 0
    double res = 0;
    char* endptr;
    res = strtod(inp,&endptr);
    if ( strcmp(endptr,"\0") == 0  ) {
        return 1;
    }
    return 0;
}


int is_dig (char inp[]){         // if inp is double or int it returns 1, if not returns 0

    if (  char_doub(inp) || isNumber(inp)    ){
        return 1;
    }
    return 0;
}

int is_op (char inp[]) {     // if inp is an operator it returns 1 else 0
    if ( strcmp(inp,"+") == 0 || strcmp(inp,"-") == 0  || strcmp(inp,"*") == 0) {
        return 1;
    }
    return 0;
}


//function to delete trailing spaces in string
char *strstrip(char *s){
    size_t size;
    char *end;
    size = strlen(s);
    if (!size)
        return s;
    end = s + size - 1;
    while (end >= s && isspace(*end))
        end--;
    *(end + 1) = '\0';

    while (*s && isspace(*s))
        s++;

    return s;
}

//function for evaluating choose statement
void* choose(void* expr1, void* expr2, void* expr3, void* expr4) {  
    if (expr1 == 0) {                                              
        return expr2;
    }
    else if (expr1 > 0) {
        return expr3;
    }
    else {
        return expr4;
    }
}




// function to assign matrix values
void assignMatrix(double value[256], struct Matrix* matrix, int size){
    for (int j = 0 ; j < size ; j++){
        (*matrix).value[j] = value[j];
    }
}
//assignMatrix(val, &matrices[i], matrices[i].size);

void produceMatrix(double adj[], double m1[], double m2[] , int m1e_cols, int m1e_rows, int m2e_cols, int m2e_rows) {

    // checks the premise required for multiplication to see if one's column is equal to the other's row
    if ( m1e_rows  ==  m2e_cols ) {

        // it takes it as a one dimensional array but the product is 2 dimensional here it creates a local 2 dimensional array
        double lm1[m1e_rows][m1e_cols];
        int ct1 = 0;
        for (int i = 0; i < m1e_rows ; i++) {
            for (int j = 0; j < m1e_cols ; j++) {
                lm1[i][j] = m1[ct1];
                ct1++;
            }
        }

        // same with the upper one
        double lm2[m2e_rows][m2e_cols];
        int c2 = 0;
        for (int i = 0; i < m2e_rows ; i++) {
            for (int j = 0; j < m2e_cols ; j++) {
                lm2[i][j] = m2[c2];
                c2++;
            }
        }

        // Here we assign the number of col and row to the first 2 indexes of the empty adj, then to be able to make it to 2 dimensions.
        adj[0] = (double) m2e_rows;
        adj[1] =  (double) m1e_cols;
        int counter = 1;

        // multiplication
        for (int c1 = 0; c1< m1e_cols ; c1++) {

            for (int r2 = 0; r2< m2e_rows ; r2++) {

                double res = 0;
                for (int i = 0; i< m1e_rows ; i++ ) {
                    res += (lm1[i][c1] * lm2[r2][i]);

                }
                counter++;
                adj[counter] = res;
            }
        }
    }
        // 0. index becomes -1 if multiplication did not occur
    else {
        adj[0] = (double) -1;
        printf("Error (Line %d)\n", linenum );
        error = 1;
    }

}

void addMatrix(double adj[], double m1[], double m2[] , int m1e_cols, int m1e_rows, int m2e_cols, int m2e_rows) {
    int size = ( m1e_cols * m2e_rows ) ;
    // For addition, the dimensions of both matrices must be equal, checking it
    if ( ( m1e_cols == m2e_cols ) && ( m1e_rows == m2e_rows ) ) {
        // adding the sum to the adjoining array
        for (int i = 0; i<size ; i++) {
            adj[i] = m1[i] + m2[i];

        }
    }
        // if there is no addition, index 0 will be equal to the this number.
    else{
        adj[0] = -99999.99;
        printf("Error (Line %d)\n", linenum );
        error = 1;
    }
}

void subtractMatrix(double adj[], double m1[], double m2[] , int m1e_cols, int m1e_rows, int m2e_cols, int m2e_rows) {
    int size = ( m1e_cols * m2e_rows ) ;
    // for subtraction, the dimensions of both matrices must be equal, checking it
    if ( ( m1e_cols == m2e_cols ) && ( m1e_rows == m2e_rows ) ) {
        // adds subtraction to adjoining array, subtracts 2 from 1
        for (int i = 0; i<size ; i++) {
            adj[i] = m1[i] - m2[i];

        }
    }
        // if there is no subtraction, index 0 will be equal to the this number.
    else{
        adj[0] = -99999.99;
        printf("Error (Line %d)\n", linenum );
        error = 1;
    }
}

void produceWithScalar(double adj[], double m[], int size, double scalar){
    for (int i = 0; i<size; i++) {
        adj[i] = scalar * m[i];
    }
}

// here produce also deletes the first 2 indexes, leaving only the produce indexes in the adj array
void clearMatrix(double adj[] ,double producedMatrix[]){
    int size = producedMatrix[0]*producedMatrix[1];
    int count = 1;
    for (int i = 0; i<size; i++) {
        count++;
        adj[i] = producedMatrix[count];
    }
}
// to find the number of columns in the produce
int numcolumns(double* producedMatrix){
    return (int) producedMatrix[0];
}
//  to find the number of rows in the produce
int numrows(double* producedMatrix){
    return (int) producedMatrix[1];
}


void evaluatematrix ( char ma[90], char mname[20], int *first, int* second ) { // matrix name separates 1st number as 2nd number
    *first = -1;    // if it can't, it will stay like this and we'll know it's a mistake
    *second = -2;   // if it is written wrong, it will stay -2, if vector is -1, if normal, as many as
    int count = 0; // to count places without spaces
    char currmname[20] = "";    // str will return to matrix name
    char firstnum[20] = "";          // first number of the matrix
    char secondnum[20] = "";         // second number of the matrix
    int virgul = 0; // 

    for (int i = 0; i<80; i++) {  
        if (ma[i] != NULL) {
            count++;
        }
    }

    for (int i = 0; i<count ; i++){
        if (ma[i]=='[') {
            for (int j = 0; j<i ;j++){
                char xd[2] = {ma[j],'\0'}; 
                strcat(currmname,xd); //name of matrix
            }
        }
        if (ma[i] == ','){ 
            virgul = 1;
            for (int j = i-1; ma[j]!='[' ; j-- ) {
                char xd[2] = {ma[j],'\0'}; // // Since the character doesn't strcat, I created str and dedicate it to it, but at the end it should be /0
                strcat(firstnum,xd);
            }
        }
        if (ma[i] == ']'){  
            if (virgul) {
                for (int j = i - 1; ma[j] != ','; j--) {
                    char xd[2] = {ma[j],'\0'}; // Since the character doesn't strcat, I created str and dedicate it to it, but at the end it should be /0
                    strcat(secondnum, xd);
                }
            }
            else{
                for (int j = i-1; ma[j]!='[' ; j-- ) {
                    char xd[2] = {ma[j],'\0'}; // Since the character doesn't strcat, I created str and dedicate it to it, but at the end it should be /0
                    strcat(firstnum,xd);
                }

            }
        }
    }
    strcpy(mname,currmname);
    strrev(firstnum);
    strrev(secondnum);

    if ( isNumber(firstnum) ) { 
        *first = atoi(firstnum);
    }
    if ( isNumber(secondnum) ) { 
        *second = -1;
        if (strcmp(secondnum,"\0")!=0) {
            *second = atoi(secondnum);
        }
    }
}

int whatres ( char arrexp[256][20] ) {  //Returns 1 if expression has only scalar expressions
    for (int i = 0; i<20; i++){

        for(int beg = 0; beg < sizeof(matrices)/sizeof(matrices[0]); beg++){
            char* name3 = (char*) matrices[beg].name;
            if (name3 != (NULL) ) {
                if (strcmp(name3, arrexp[i]) == 0){
                    return 0;                   
                }
            }
        }
        if ( strcmp(arrexp[i+1],"\0") == 0 ) {
            break;
        }
    }
    return 1;
}

//function that allows to assign the value of vector and matrix in that index directly to where it is in expression
void editscalar( char arrexp[256][20] )    { 

    for (int i = 0; i<80; i++) {

        if ( !is_dig(arrexp[i]) && !is_op(arrexp[i]) ) {
            for (int beg = 0; beg < sizeof(scalars) / sizeof(scalars[0]); beg++) {
                char *name3 = (char *) scalars[beg].name;
                if (name3 != (NULL)) {

                    if (strcmp(name3, arrexp[i]) == 0) {
                        char output[20];
                        snprintf(output, 20, "%f", scalars[beg].d_value);
                        strcpy(arrexp[i] ,output);
                    }
                }
            }
            int f1; int f2;
            int* pf1 = &f1; int* pf2 = &f2;
            char mn[20];
            evaluatematrix(arrexp[i], mn, pf1, pf2); // matrix name 1st number split as 2nd number
            for (int beg = 0; beg < sizeof(matrices) / sizeof(matrices[0]); beg++) {
                char *name3 = (char *) matrices[beg].name;
                if (name3 != (NULL)) {

                    if (strcmp(name3, mn) == 0) {

                        if (f1 != -1){
                            if (f2 == -2){ 
                                printf("Error matrix ici sacmalik");
                            }
                            else if (f2 == -1) { // vector
                                char output[20];
                                snprintf(output, 20, "%f", matrices[beg].value[f1-1]); 
                                strcpy(arrexp[i] ,output);

                            }
                            else { // matrix
                                char output[20];
                                int correspondingind = ( (f1-1) * matrices[beg].e_rows ) + f2 - 1;  // Since it is one-dimensional, it calculates the corresponding index (index starts from 1)
                                snprintf(output, 20, "%f", matrices[beg].value[correspondingind]);    
                                strcpy(arrexp[i] ,output);
                            }
                        }
                        else {
                            printf("Error: matrix ici sacmalik");
                        }
                    }
                }
            }
        }
    }
}

double resultt ( char arrexp[256][20]) {    // Calculates the operations and returns the result

    int op = 1;             // controller so that firstop doesn't always change
    int counter = 0;       // counter that places numbers
    int lastind;          // last index
    int firstopind;      // 1st operator index at the end
    int beforeopnum = -1 ;  

    for (int i = 0; i<80; i++){
        if ( strcmp(arrexp[i+1],"\0") == 0 ) { // if end of array get last index stop loop
            lastind = i;
            if (is_dig(arrexp[i-1])) { // If it's a number before the last index, it's a 3-element expression.
                firstopind = i;
                counter+=2;
            }
            break;
        }

        if ( is_dig(arrexp[i]) ) {  // if a digit
            if ( is_dig(arrexp[i+1]) && is_dig(arrexp[i+2]) ) {  //  if the next two indexes are numbers, move this number to the counter index of the array
                strcpy(arrexp[counter],arrexp[i]);
                counter++;
            }
        }
        if ( !is_dig(arrexp[i]) && !is_op(arrexp[i]) ) {
            for (int beg = 0; beg < sizeof(matrices) / sizeof(matrices[0]); beg++) {
                char *name3 = (char *) matrices[beg].name;
                if (name3 != (NULL)) {
                    if (strcmp(name3, arrexp[i]) == 0) {
                    //matrix
                    }
                }
            }

            for (int beg = 0; beg < sizeof(scalars) / sizeof(scalars[0]); beg++) {
                char *name3 = (char *) scalars[beg].name;
                if (name3 != (NULL)) {
                    if (strcmp(name3, arrexp[i]) == 0) {
                        //it means scalar
                    }
                }
            }
        }

        if (strcmp(arrexp[i], "sqrt")==0){  //sqrt function

        }

        if (strcmp(arrexp[i], "tr")==0){  //transpose function
        }

        if (strcmp(arrexp[i], "choose")==0){  //choose function

        }


        if (is_op(arrexp[i])) { // if it is an operator

            if (is_dig(arrexp[i-1]) && is_dig(arrexp[i-2])) { // If the previous two are digits, it is the operator with the current precedence

                if ( strcmp(arrexp[i],"+") == 0 ) { // if the operator is +
                    char * endptr1;
                    char * endptr2;
                    double firstoperand = strtod(arrexp[i-2],&endptr1); // converted char arrays to double
                    double secondoperand = strtod(arrexp[i-1],&endptr2);
                    double res = firstoperand + secondoperand;
                    char output[20];
                    snprintf(output, 20, "%f", res);
                    strcpy(arrexp[counter],output);
                    counter++;
                }
                if ( strcmp(arrexp[i],"*") == 0 ) { // if the operator is *
                    char * endptr1;
                    char * endptr2;
                    double firstoperand = strtod(arrexp[i-2],&endptr1);
                    double secondoperand = strtod(arrexp[i-1],&endptr2);
                    double res = firstoperand * secondoperand;
                    char output[20];
                    snprintf(output, 20, "%f", res);
                    strcpy(arrexp[counter],output);
                    counter++;
                }
                if ( strcmp(arrexp[i],"-") == 0 ) { // if the operator is -
                    char * endptr1;
                    char * endptr2;
                    double firstoperand = strtod(arrexp[i-2],&endptr1);
                    double secondoperand = strtod(arrexp[i-1],&endptr2);
                    double res = firstoperand - secondoperand;
                    char output[20];
                    snprintf(output, 20, "%f", res);
                    strcpy(arrexp[counter],output);
                    counter++;
                }

            }
            else {  // If either of the previous two is not a number, it is not the operator to enter this operation.
                if ( op == 1) {
                    firstopind = i;
                    op = 0;
                    if ( is_dig(arrexp[i-1]) && !is_dig(arrexp[i-2]) ) { // If there is a single number before this operator, save its index and add it to the last counter
                        beforeopnum = i-1;
                    }
                }
            }
        }
    }
    if (beforeopnum != -1) {   
        strcpy(arrexp[counter], arrexp[beforeopnum]);
        counter++;
    }

    char * endptrxx;
    double res = strtod(arrexp[0],&endptrxx); 

    for (int i = 1; i<counter; i++){
        if ( strcmp(arrexp[lastind],"+") == 0 ) { // if the operator is +
            char * endptr1;
            double firstoperand = strtod(arrexp[i],&endptr1);
            res += firstoperand;
            lastind--;
            if (lastind == firstopind-1){
                break;
            }
        }
        else if ( strcmp(arrexp[lastind],"-") == 0 ) { // if the operator is -
            char * endptr1;
            double firstoperand = strtod(arrexp[i],&endptr1);
            res -= firstoperand;
            lastind--;
            if (lastind == firstopind-1){
                break;
            }
        }
        else if ( strcmp(arrexp[lastind],"*") == 0 ) { // if the operator is *
            char * endptr1;
            double firstoperand = strtod(arrexp[i],&endptr1);
            res *= firstoperand;
            lastind--;
            if (lastind == firstopind-1){
                break;
            }
        }

    }
    return res;
}


void resultarr ( double adj[200],  char arrexp[256][20]  ) {    // calculates the operations returns the result to the adj

    int op = 1;             // controller for first op
    int counter = 0;       // counter which put the numbers
    int lastind;          // last index
    int firstopind;      // 1st operator index at the end
    int beforeopnum = -1 ;  

    for (int i = 0; i<80; i++){

        int beforeis_2m = 0; 
        int beforeis_m = 0; 
        int after1m = 0;    
        int after2m = 0;    
        int is_m = 0;       
        for (int beg = 0; beg < sizeof(matrices) / sizeof(matrices[0]); beg++) {
            char *name3 = (char *) matrices[beg].name;
            if (name3 != (NULL)) {
                if (strcmp(name3, arrexp[i]) == 0) {
                    is_m = 1;                    //matrix 
                }
                if (strcmp(name3, arrexp[i-1]) == 0) {
                    beforeis_m = 1;                    //matrix 
                }
                if (strcmp(name3, arrexp[i-2]) == 0) {
                    beforeis_2m = 1;                    //matrix 
                }
                if (strcmp(name3, arrexp[i+1]) == 0) {
                    after1m= 1;                    //matrix 
                }
                if (strcmp(name3, arrexp[i+2]) == 0) {
                    after2m = 1;                    //matrix 
                }
            }
        }


        if ( strcmp(arrexp[i+1],"\0") == 0 ) { // if array ends, take the last index and break the loop
            lastind = i;
            if (is_dig(arrexp[i-1]) || beforeis_m==1 ) {
                firstopind = i;
                counter+=2;
            }
            break;
        }

        if ( is_dig(arrexp[i]) || is_m ) {  // if matrix or number
            if ( ( is_op(arrexp[i+1]) != 1)  && ( is_op(arrexp[i+2]) != 1) ) {  
                strcpy(arrexp[counter],arrexp[i]);
                counter++;
            }
        }


        if (strcmp(arrexp[i], "sqrt")==0){  //sqrt

        }

        if (strcmp(arrexp[i], "tr")==0){  //transpose 
        }

        if (strcmp(arrexp[i], "choose")==0){  //choose 

        }


        if (is_op(arrexp[i])) { // if operator

            if (is_dig(arrexp[i-1]) && is_dig(arrexp[i-2])) { // if last two index is digit, array[i] is precedence operator 

                if ( strcmp(arrexp[i],"+") == 0 ) { // if operator is +
                    char * endptr1;
                    char * endptr2;
                    double firstoperand = strtod(arrexp[i-2],&endptr1); // convert char arrays to double
                    double secondoperand = strtod(arrexp[i-1],&endptr2);
                    double res = firstoperand + secondoperand;
                    char output[20];
                    snprintf(output, 20, "%f", res);   // convert double to char array :D
                    strcpy(arrexp[counter],output);
                    counter++;
                }
                if ( strcmp(arrexp[i],"*") == 0 ) { // if operator is *
                    char * endptr1;
                    char * endptr2;
                    double firstoperand = strtod(arrexp[i-2],&endptr1);
                    double secondoperand = strtod(arrexp[i-1],&endptr2);
                    double res = firstoperand * secondoperand;
                    char output[20];
                    snprintf(output, 20, "%f", res);
                    strcpy(arrexp[counter],output);
                    counter++;
                }
                if ( strcmp(arrexp[i],"-") == 0 ) { // if operator is -
                    char * endptr1;
                    char * endptr2;
                    double firstoperand = strtod(arrexp[i-2],&endptr1);
                    double secondoperand = strtod(arrexp[i-1],&endptr2);
                    double res = firstoperand - secondoperand;
                    char output[20];
                    snprintf(output, 20, "%f", res);
                    strcpy(arrexp[counter],output);
                    counter++;
                }

            }
            if ( beforeis_m == 1 && is_dig(arrexp[i-2]) ) { // if last one is matrix and before that is scalar
                if (strcmp(arrexp[i],"*") == 0) {   
                    char * endptr;
                    double carpan = strtod(arrexp[i-2],&endptr);
                    int ind = -1;
                    for (int beg = 0; beg < sizeof(matrices) / sizeof(matrices[0]); beg++) { 
                        char *name3 = (char *) matrices[beg].name;
                        if (name3 != (NULL)) {
                            if (strcmp(name3, arrexp[i-1]) == 0) {
                                ind = beg;
                            }
                        }
                    }
                    if ( ind != -1 ) {
                        int size = matrices[ind].e_rows * matrices[ind].e_cols;
                        double adj[size];
                        produceWithScalar(adj, matrices[ind].value, size, carpan);
                        char currname[20];
                        sprintf(currname,"%d",producedmatrixind);  // do that for making each name will be distinct
                        strcat(currname,"adj");                 
                        matrices[matrixind].name = currname;    // making a new struct which will be used in next operations to get the result
                        matrices[matrixind].e_rows = matrices[ind].e_rows;
                        matrices[matrixind].e_cols = matrices[ind].e_cols;
                        assignMatrix(adj,&matrices[matrixind],size);
                        matrixind++;
                        producedmatrixind++;
                        strcpy(arrexp[counter],currname);
                        counter++;

                    }

                }

            }
            if ( beforeis_2m == 1 && is_dig(arrexp[i-1]) ) {    // if last one is scalar and before that is matrix
                if (strcmp(arrexp[i],"*") == 0) {
                    char * endptr;
                    double carpan = strtod(arrexp[i-1],&endptr);
                    int ind = -1;
                    for (int beg = 0; beg < sizeof(matrices) / sizeof(matrices[0]); beg++) {
                        char *name3 = (char *) matrices[beg].name;
                        if (name3 != (NULL)) {
                            if (strcmp(name3, arrexp[i-2]) == 0) {
                                ind = beg;
                            }
                        }
                    }
                    if ( ind != -1 ) {
                        int size = matrices[ind].e_rows * matrices[ind].e_cols;
                        double adj[size];
                        produceWithScalar(adj, matrices[ind].value, size, carpan);
                        char currname[20];
                        sprintf(currname,"%d",producedmatrixind); 
                        strcat(currname,"adj");
                        matrices[matrixind].name = currname;    
                        matrices[matrixind].e_rows = matrices[ind].e_rows;
                        matrices[matrixind].e_cols = matrices[ind].e_cols;
                        assignMatrix(adj,&matrices[matrixind],size);
                        matrixind++;
                        producedmatrixind++;
                        strcpy(arrexp[counter],currname);
                        counter++;
                    }
                }
            }
            if ( beforeis_m == 1 && beforeis_2m == 1 ){ // if both last one and the before that arrexp indices were matrices
                if (strcmp(arrexp[i],"*") == 0) {
                    int ind1 = -1;  // first matrix index to enter the operation
                    int ind2 = -1;  // second matrix index to enter the operation
                    for (int beg = 0; beg < sizeof(matrices) / sizeof(matrices[0]); beg++) {
                        char *name3 = (char *) matrices[beg].name;
                        if (name3 != (NULL)) {
                            if (strcmp(name3, arrexp[i-2]) == 0) {
                                ind1 = beg;
                            }
                            if (strcmp(name3, arrexp[i-1]) == 0) {
                                ind2 = beg;
                            }
                        }
                    }
                    if (ind1 != -1 && ind2 != -1) {
                        double pMatrix[ matrices[ind1].e_cols * matrices[ind2].e_rows + 2]; // adjacent matrices index should be more 2 of the product of row and column numbers for produce function
                        produceMatrix(pMatrix,matrices[ind1].value,matrices[ind2].value,matrices[ind1].e_cols,matrices[ind1].e_rows,matrices[ind2].e_cols,matrices[ind2].e_rows);
                        if (pMatrix[0] != -1) {
                            char currname[20];
                            sprintf(currname, "%d",producedmatrixind); 
                            strcat(currname, "adj");
                            matrices[matrixind].name = currname;   
                            matrices[matrixind].e_rows = numcolumns(pMatrix);
                            matrices[matrixind].e_cols = numrows(pMatrix);
                            double cMatrix[numcolumns(pMatrix) * numrows(pMatrix)];
                            clearMatrix(cMatrix,pMatrix);
                            assignMatrix(cMatrix, &matrices[matrixind], numcolumns(pMatrix) * numrows(pMatrix));
                            matrixind++;
                            producedmatrixind++;
                            strcpy(arrexp[counter],currname);
                            counter++;
                        }
                    }
                }
                if (strcmp(arrexp[i],"+") == 0) {
                    int ind1 = -1;  
                    int ind2 = -1; 
                    for (int beg = 0; beg < sizeof(matrices) / sizeof(matrices[0]); beg++) {
                        char *name3 = (char *) matrices[beg].name;
                        if (name3 != (NULL)) {
                            if (strcmp(name3, arrexp[i-2]) == 0) {
                                ind1 = beg;
                            }
                            if (strcmp(name3, arrexp[i-1]) == 0) {
                                ind2 = beg;
                            }
                        }
                    }
                    if (ind1 != -1 && ind2 != -1) {
                        double pMatrix[ matrices[ind1].e_cols * matrices[ind2].e_rows ]; 
                        addMatrix(pMatrix,matrices[ind1].value,matrices[ind2].value,matrices[ind1].e_cols,matrices[ind1].e_rows,matrices[ind2].e_cols,matrices[ind2].e_rows);
                        if (pMatrix[0] != -99999.99) {
                            char currname[20];
                            sprintf(currname, "%d",producedmatrixind);  
                            strcat(currname, "adj");
                            matrices[matrixind].name = currname;   
                            matrices[matrixind].e_rows = matrices[ind2].e_rows;
                            matrices[matrixind].e_cols = matrices[ind1].e_cols;
                            assignMatrix(pMatrix, &matrices[matrixind], matrices[matrixind].e_rows * matrices[matrixind].e_cols );
                            matrixind++;
                            producedmatrixind++;
                            strcpy(arrexp[counter],currname);
                            counter++;
                        }
                    }
                }
                if (strcmp(arrexp[i],"-") == 0) {
                    int ind1 = -1; 
                    int ind2 = -1; 
                    for (int beg = 0; beg < sizeof(matrices) / sizeof(matrices[0]); beg++) {
                        char *name3 = (char *) matrices[beg].name;
                        if (name3 != (NULL)) {
                            if (strcmp(name3, arrexp[i-2]) == 0) {
                                ind1 = beg;
                            }
                            if (strcmp(name3, arrexp[i-1]) == 0) {
                                ind2 = beg;
                            }
                        }
                    }
                    if (ind1 != -1 && ind2 != -1) {
                        double pMatrix[ matrices[ind1].e_cols * matrices[ind2].e_rows ];
                        subtractMatrix(pMatrix,matrices[ind1].value,matrices[ind2].value,matrices[ind1].e_cols,matrices[ind1].e_rows,matrices[ind2].e_cols,matrices[ind2].e_rows);
                        if (pMatrix[0] != -99999.99) {
                            char currname[20];
                            sprintf(currname, "%d",producedmatrixind);  
                            strcat(currname, "adj");
                            matrices[matrixind].name = currname;  
                            matrices[matrixind].e_rows = matrices[ind2].e_rows;
                            matrices[matrixind].e_cols = matrices[ind1].e_cols;
                            assignMatrix(pMatrix, &matrices[matrixind], matrices[matrixind].e_rows * matrices[matrixind].e_cols);
                            matrixind++;
                            producedmatrixind++;
                            strcpy(arrexp[counter],currname);
                            counter++;
                        }
                    }
                }

            }
                    
            else {  // if non of the before two indices were a digit or matrices that was not the operator to enter the operation now 
                if ( op == 1) { // if we didn't assign it before it enter the if statement
                    firstopind = i;
                    op = 0;
                    if ( !is_op(arrexp[i-1]) && is_op(arrexp[i-2]) ) { // if there is a digit or matrix before that operator save it to use it later
                        beforeopnum = i-1;
                    }
                }
            }
        }
    }
    if (beforeopnum != -1) {    // if there is a digit or matrix before the operator which wouldn't be used now that thing goes to char array
        strcpy(arrexp[counter], arrexp[beforeopnum]);
        counter++;
    }

    


    for (int i = 1; i<counter; i++){


        if (strcmp(arrexp[lastind],"+") == 0 ) {
            int ind1 = -1;  
            int ind2 = -1;  
            for (int beg = 0; beg < sizeof(matrices) / sizeof(matrices[0]); beg++) {
                char *name3 = (char *) matrices[beg].name;
                if (name3 != (NULL)) {
                    if (strcmp(name3, arrexp[0]) == 0) {
                        ind1 = beg;
                    }
                    if (strcmp(name3, arrexp[i]) == 0) {
                        ind2 = beg;
                    }
                }
            }
            if (ind1 != -1 && ind2 != -1) {
                double pMatrix[ matrices[ind1].e_cols * matrices[ind2].e_rows ]; 
                addMatrix(pMatrix,matrices[ind1].value,matrices[ind2].value,matrices[ind1].e_cols,matrices[ind1].e_rows,matrices[ind2].e_cols,matrices[ind2].e_rows);
                if (pMatrix[0] != -99999.99) {
                    char currname[20];
                    sprintf(currname, "%d",producedmatrixind);  
                    strcat(currname, "adj");
                    matrices[matrixind].name = currname;    
                    matrices[matrixind].e_rows = matrices[ind2].e_rows;
                    matrices[matrixind].e_cols = matrices[ind1].e_cols;
                    assignMatrix(pMatrix, &matrices[matrixind], matrices[matrixind].e_rows * matrices[matrixind].e_cols);
                    matrixind++;
                    producedmatrixind++;
                    strcpy(arrexp[0],currname);
                    lastind--;
                    if (lastind == firstopind-1){
                        break;
                    }
                }
            }
        }

        else if ( strcmp(arrexp[lastind],"-") == 0 ) {
            int ind1 = -1; 
            int ind2 = -1;  
            for (int beg = 0; beg < sizeof(matrices) / sizeof(matrices[0]); beg++) {
                char *name3 = (char *) matrices[beg].name;
                if (name3 != (NULL)) {
                    if (strcmp(name3, arrexp[0]) == 0) {
                        ind1 = beg;
                    }
                    if (strcmp(name3, arrexp[i]) == 0) {
                        ind2 = beg;
                    }
                }
            }
            if (ind1 != -1 && ind2 != -1) {
                double pMatrix[ matrices[ind1].e_cols * matrices[ind2].e_rows ]; 
                subtractMatrix(pMatrix,matrices[ind1].value,matrices[ind2].value,matrices[ind1].e_cols,matrices[ind1].e_rows,matrices[ind2].e_cols,matrices[ind2].e_rows);
                if (pMatrix[0] != -99999.99) {
                    char currname[20];
                    sprintf(currname, "%d",producedmatrixind); 
                    strcat(currname, "adj");
                    matrices[matrixind].name = currname;    
                    matrices[matrixind].e_rows = matrices[ind2].e_rows;
                    matrices[matrixind].e_cols = matrices[ind1].e_cols;
                    assignMatrix(pMatrix, &matrices[matrixind], matrices[matrixind].e_rows * matrices[matrixind].e_cols);
                    matrixind++;
                    producedmatrixind++;
                    strcpy(arrexp[0],currname);
                    lastind--;
                    if (lastind == firstopind-1){
                        break;
                    }
                }
            }
        }

        else if ( strcmp(arrexp[lastind],"*") == 0 ) { 
            if (is_dig(arrexp[i])) {
                char *endptr;
                double carpan = strtod(arrexp[i], &endptr);
                int ind = -1;
                for (int beg = 0; beg < sizeof(matrices) / sizeof(matrices[0]); beg++) {
                    char *name3 = (char *) matrices[beg].name;
                    if (name3 != (NULL)) {
                        if (strcmp(name3, arrexp[0]) == 0) {
                            ind = beg;
                        }
                    }
                }
                if (ind != -1) {
                    int size = matrices[ind].e_rows * matrices[ind].e_cols;
                    double adj[size];
                    produceWithScalar(adj, matrices[ind].value, size, carpan);
                    char currname[20];
                    sprintf(currname, "%d",producedmatrixind);  
                    strcat(currname, "adj");
                    matrices[matrixind].name = currname;    
                    matrices[matrixind].e_rows = matrices[ind].e_rows;
                    matrices[matrixind].e_cols = matrices[ind].e_cols;
                    assignMatrix(adj, &matrices[matrixind], size);
                    matrixind++;
                    producedmatrixind++;
                    strcpy(arrexp[0], currname);
                    lastind--;
                    if (lastind == firstopind - 1) {
                        break;
                    }
                }
            }
            else {
                int ind1 = -1;  // first matrix arrexp[0]'s index
                int ind2 = -1;  // second matrix yani arrexp[i]'s index
                for (int beg = 0; beg < sizeof(matrices) / sizeof(matrices[0]); beg++) {
                    char *name3 = (char *) matrices[beg].name;
                    if (name3 != (NULL)) {
                        if (strcmp(name3, arrexp[0]) == 0) {
                            ind1 = beg;
                        }
                        if (strcmp(name3, arrexp[i]) == 0) {
                            ind2 = beg;
                        }
                    }
                }
                if (ind1 != -1 && ind2 != -1) {
                    double pMatrix[ matrices[ind1].e_cols * matrices[ind2].e_rows + 2]; 
                    produceMatrix(pMatrix,matrices[ind1].value,matrices[ind2].value,matrices[ind1].e_cols,matrices[ind1].e_rows,matrices[ind2].e_cols,matrices[ind2].e_rows);
                    if (pMatrix[0] != -1) {
                        char currname[20];
                        sprintf(currname, "%d",producedmatrixind);  
                        strcat(currname, "adj");
                        matrices[matrixind].name = currname;   
                        matrices[matrixind].e_rows = numcolumns(pMatrix);
                        matrices[matrixind].e_cols = numrows(pMatrix);
                        double cMatrix[numcolumns(pMatrix) * numrows(pMatrix)]; 
                        clearMatrix(cMatrix,pMatrix);
                        assignMatrix(cMatrix, &matrices[matrixind], numcolumns(pMatrix) * numrows(pMatrix));
                        matrixind++;
                        producedmatrixind++;
                        strcpy(arrexp[0],currname);
                        lastind--;
                        if (lastind == firstopind - 1) {
                            break;
                        }
                    }
                }
            }
        }
    }
    int ind1 = -1;  // first matrix arrexp[0]'s index
    for (int beg = 0; beg < sizeof(matrices) / sizeof(matrices[0]); beg++) {
        char *name3 = (char *) matrices[beg].name;
        if (name3 != (NULL)) {
            if (strcmp(name3, arrexp[0]) == 0) {
                ind1 = beg;
            }
        }
    }
    if (ind1 != -1) {
        for (int k = 0; k< matrices[ind1].e_rows * matrices[ind1].e_cols; k++){
            adj[k] = matrices[ind1].value[k];
        }
    }
}

    
//parsing the expressions
int expr(char *str){

    char str1[N], str2[N] ;
    
    str1[0] = str2[0] = '\0' ;
    
    if (!term(str1)) {
        return(0) ;
    }
    if (!moreterms(str2)) {
        return(0) ;
    }
    strcat(str1,str2) ;
    strcpy(str,str1) ;
    
    return(1) ;
}

int term(char *str){

    char str1[N], str2[N] ;

    str1[0] = str2[0] = '\0' ;
    if (!factor(str1)) {
        return(0) ;
    }
    if (!morefactors(str2)) {
        return(0) ;
    }

    strcat(str1,str2) ;
    strcpy(str,str1) ;
    return(1) ;
}

int moreterms(char *str){
    char str1[N], str2[N], str3[N] ;

    str1[0] = str2[0] = str3[0] = '\0' ;

    if ( (strcmp(expressions[cur],"+") == 0 ) || (strcmp(expressions[cur],"-") == 0 ) ) {
        strcpy(str1,expressions[cur]) ;
        strcat(str1," ") ;
        
        cur++ ;
        if (!term(str2)) {
            return(0) ;
        }
        if (!moreterms(str3)) {
            return(0) ;
        }
    }
    strcat(str2,str3) ;
    strcat(str2,str1) ;
    
    strcpy(str,str2) ;
    return(1) ;
}

int factor(char *str){  
    char str1[N] ;
    str1[0] = '\0' ;


    if (is_integer(expressions[cur])) { //num
        strcpy(str,expressions[cur]) ;
        strcat(str," ") ;
        cur++ ;
        return(1) ;
    }


    if (strcmp(expressions[cur],"(") == 0 ) { //(expr)
        cur++ ;
        if (!expr(str1) ) {
            return(0) ;
        }
        if (strcmp(expressions[cur],")") != 0 ) {
            printf("Error: expecting paranthesis\n") ;
            return(0) ;
        }
        cur++ ;

        strcpy(str,str1) ;
        return(1) ;
    }

    if(data[cur+2] == 24 && data[cur+3] != 8){  //id
        strcpy(str,expressions[cur]) ;
        strcat(str," ") ;
        cur++ ;
        return(1) ;
    }

    if(data[cur+2] == 24 && data[cur+3] == 8 && data[cur+5] != 14){  //id[expr]
        cur++;
        if (strcmp(expressions[cur],"[") == 0 ) {
            cur++;
            if (!expr(str1) ) {
                return(0) ;
            }

            if (strcmp(expressions[cur],"]") != 0  ) {
                printf("Error: expecting paranthesis\n") ;
                return(0) ;
            }
            cur++ ;
            char matrx[30];   
            strcpy(matrx,expressions[cur-4]);
            strcat(matrx,"[");
            strstrip(str1);
            strcat(matrx,str1);
            strcat(matrx,"]");
            strcpy(str, matrx);
            strcat(str, " ");
            return(1) ;
        }
    }

    if(data[cur+2] == 24 && data[cur+3] == 8 && data[cur+5] == 14){  //id[expr,expr]

        char str2[N] ;
        str2[0] = '\0' ;

        cur++;
        if (strcmp(expressions[cur],"[") == 0 ) {
            cur++;

            if (!expr(str1) ) {
                return(0) ;
            }

            if (strcmp(expressions[cur],",") != 0  ) {
                printf("Error: expecting comma\n") ;
                return(0) ;
            }
            cur++;

            if (!expr(str2) ) {
                return(0) ;
            }

            if (strcmp(expressions[cur],"]") != 0  ) {
                printf("Error: expecting paranthesis\n") ;
                return(0) ;
            }
            cur++ ;
            char matrx[30];   
            strcpy(matrx,expressions[cur-6]);
            strcat(matrx,"[");
            strstrip(str1);
            strcat(matrx,str1);
            strcat(matrx,",");
            strstrip(str2);
            strcat(matrx,str2);
            strcat(matrx,"]");
            strcpy(str, matrx);
            strcat(str, " ");

            return(1) ;
        }
    }

    if(data[cur+2] == 20){  //choose(expr1,expr2,expr3,expr4)
        char str2[N] ;
        str2[0] = '\0' ;
        char str3[N] ;
        str3[0] = '\0' ;
        char str4[N] ;
        str4[0] = '\0' ;

        cur++;
        if (strcmp(expressions[cur],"(") == 0 ) {
            cur++;

            if (!expr(str1) ) {
                return(0) ;
            }

            if (strcmp(expressions[cur],",") != 0  ) {
                printf("Error: expecting comma\n") ;
                return(0) ;
            }
            cur++;

            if (!expr(str2) ) {
                return(0) ;
            }

            if (strcmp(expressions[cur],",") != 0  ) {
                printf("Error: expecting comma\n") ;
                return(0) ;
            }
            cur++;

            if (!expr(str3) ) {
                return(0) ;
            }

            if (strcmp(expressions[cur],",") != 0  ) {
                printf("Error: expecting comma\n") ;
                return(0) ;
            }
            cur++;

            if (!expr(str4) ) {
                return(0) ;
            }

            if (strcmp(expressions[cur],")") != 0  ) {
                printf("Error: expecting paranthesis\n") ;
                return(0) ;
            }
            cur++ ;

            strcpy(str, expressions[cur-10]);
            strcat(str, " ( ");
            strcat(str,str1) ;
            strcat(str,", ") ;
            strcat(str,str2) ;
            strcat(str,", ") ;
            strcat(str,str3) ;
            strcat(str,", ") ;
            strcat(str,str4) ;
            strcat(str,") ") ;
            return(1) ;
        }
    }


    if(data[cur+2] == 18){ //tr(expr)
        cur++;
        if (strcmp(expressions[cur],"(") == 0 ) {
            cur++;
            if (!expr(str1) ) {
                return(0) ;
            }

            if (strcmp(expressions[cur],")") != 0  ) {
                printf("Error: expecting paranthesis\n") ;
                return(0) ;
            }
            cur++ ;
            char trr[30];
            strcpy(trr, "tr(");
            strcat(trr,str1) ;
            strcat(trr,")") ;
            strcpy(str, trr);
            strcat(str, " ");
            return(1) ;
        }
    }

    if(data[cur+2] == 19){ //sqrt(expr)
        cur++;
        if (strcmp(expressions[cur],"(") == 0 ) {
            cur++;
            if (!expr(str1) ) {
                return(0) ;
            }

            if (strcmp(expressions[cur],")") != 0  ) {
                printf("Error: expecting paranthesis\n") ;
                return(0) ;
            }
            cur++ ;
            
            char sqrtt[30];
            strcpy(sqrtt, "sqrt(");
            strcat(sqrtt,str1) ;
            strcat(sqrtt,")") ;
            strcpy(str, sqrtt);
            strcat(str, " ");
            return(1) ;
        }
    }

    printf("Error: expecting factor\n") ;
    return(0) ;
}

int morefactors(char *str){
    char str1[N], str2[N], str3[N] ;

    str1[0] = str2[0] = str3[0] = '\0' ;

    if ( (strcmp(expressions[cur],"*") == 0 ) || (strcmp(expressions[cur],"/") == 0 ) ) {
        strcpy(str1,expressions[cur]) ;
        strcat(str1," ") ;
        
        cur++ ;
        if (!  factor(str2)) {
            return(0) ;
        }
        if (!  morefactors(str3)) {
            return(0) ;
        }
    }
    strcat(str2,str3) ;
    strcat(str2,str1) ;
    strcpy(str,str2) ;
    return(1) ;
}

//function for detecting the factor is integer
int is_integer(char *token) {
    int isnumber = 1 ;
    char *q ;

    for(q = token ; *q != '\0' ; q++) {
        isnumber = isnumber && isdigit(*q) ;
    }
    return(isnumber) ;
}


// this function add blanks to the char array arr if there is a special character
void parseline(char line[]){
    int u = strlen(line);
    for(int i=0; i < u; i++){
        if ( line[i] == '+'|| line[i] == '-'|| line[i] == '*'|| line[i] == '('|| line[i] == ')'|| line[i] == '['|| line[i] == ']'|| line[i] == '='|| line[i] == '{'|| line[i] == '}'||
             line[i] == ':'|| line[i] == ','){
            strcat(arr, space);
            char c = line[i];
            char str1[2] = {c, '\0'};
            strcat(arr, str1);
            strcat(arr, space);
        }
        else{
            char c = line[i];
            char str1[2] = {c, '\0'};
            strcat(arr, str1);
        }
    }
}


int main (int argc,char *argv[]) {

    FILE *fp;  //pointer for this file
    FILE *fptr; //pointer to output file for writing
    char filename[] = "file.c";
    char line[80];
    char str[N];
    int ret;


    if (argc != 2) {
        printf("Give filename as command line argument\n") ;
        return(1);
    }

    fp = fopen(argv[1], "r");
    
    if(fp == NULL) {
        printf("Cannot open %s\n",argv[1]);
        return(1);
    }
    
    fptr = fopen(filename, "w");
    //we wrote header files, global functions and variables for output file
    fprintf(fptr,"#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\n#include <assert.h>\n#include <ctype.h>\n#define N 300\n#define MAXTOKENS 256\n#define TOKENLENGTH 20\n");
    fprintf(fptr, "int is_int(double x){\nint z = (int) x;\nif ( x - z == 0){\n\treturn 1;\n}\nelse {\n\treturn 0;\n}\n}\n");
    fprintf(fptr, "int rowindex = 0;\n");
	fprintf(fptr, "int main (int argc){\n");
    
    
    while( fgets(line,256,fp) != NULL ) {
        memset(arr, '\0', sizeof(arr));  //clears the remainder of the previous round at the start of each line
        parseline(line);
        char* token;
        char* rest = arr;
        int a = 0;
        int k = 0;
        strstrip(rest);
        linenum++;  //this tracks our linenum for error line

        while ((token = strtok_r(rest, " ", &rest))){   //We used strtok_r because there may be multiple spaces between tokens we want to clear all of them
        
            if (strcmp(token, "for") == 0){
                data[a] = FOR;
                strcpy(tokens[a], "for");
                a = a+1;
            }
            else if (strcmp(token, "in") == 0){
                data[a] = IN;
                strcpy(tokens[a], "in");
                a = a+1;
            }
            else if (strcmp(token, "(") == 0){
                data[a] = LPARANTH;
                strcpy(tokens[a], "(");
                a = a+1;
            }
            else if (strcmp(token, ")") == 0){
                data[a] = RPARANTH;
                strcpy(tokens[a], ")");
                a = a+1;
            }
            else if (strcmp(token, "+") == 0){
                data[a] = PLUS;
                strcpy(tokens[a], "+");
                a = a+1;
            }
            else if (strcmp(token, "-") == 0){
                data[a] = MINUS;
                strcpy(tokens[a], "-");
                a = a+1;
            }
            else if (strcmp(token, "*") == 0){
                data[a] = MULTI;
                strcpy(tokens[a], "*");
                a = a+1;
            }
            else if (strcmp(token, "[") == 0){
                data[a] = LKOSELI;
                strcpy(tokens[a], "[");
                a = a+1;
            }
            else if (strcmp(token, "]") == 0){
                data[a] = RKOSELI;
                strcpy(tokens[a], "]");
                a = a+1;
            }
            else if (strcmp(token, "{") == 0){
                data[a] = LSUSLU;
                strcpy(tokens[a], "{");
                a = a+1;
            }
            else if (strcmp(token, "}") == 0){
                data[a] = RSUSLU;
                strcpy(tokens[a], "}");
                a = a+1;
            }
            else if (strcmp(token, "=") == 0){
                data[a] = EQUAL;
                strcpy(tokens[a], "=");
                a = a+1;
            }
            else if (strcmp(token, ":") == 0){
                data[a] = IKINOKTA;
                strcpy(tokens[a], ":");
                a = a+1;
            }
            else if (strcmp(token, ",") == 0){
                data[a] = VIRGUL;
                strcpy(tokens[a], ",");
                a = a+1;
            }
            else if (strcmp(token, "scalar") == 0){
                data[a] = SCALAR;
                strcpy(tokens[a], "scalar");
                a = a+1;
            }
            else if (strcmp(token, "vector") == 0){
                data[a] = VECTOR;
                strcpy(tokens[a], "vector");
                a = a+1;
            }
            else if (strcmp(token, "matrix") == 0){
                data[a] = MATRIX;
                strcpy(tokens[a], "matrix");
                a = a+1;
            }
            else if (strcmp(token, "tr") == 0){
                data[a] = TRANSPOSE;
                strcpy(tokens[a], "tr");
                a = a+1;
            }
            else if (strcmp(token, "sqrt") == 0){
                data[a] = SQRT;
                strcpy(tokens[a], "sqrt");
                a = a+1;
            }
            else if (strcmp(token, "choose") == 0){
                data[a] = CHOOSE;
                strcpy(tokens[a], "choose");
                a = a+1;
            }
            else if (strcmp(token, "#") == 0){
                data[a] = HASHTAG;
                strcpy(tokens[a], "#");
                a = a+1;
            }
            else if (strcmp(token, "print") == 0){
                data[a] = PRINT;
                strcpy(tokens[a], "print");
                a = a+1;
            }
            else if (strcmp(token, "printsep") == 0){
                data[a] = PRINTSEP;
                strcpy(tokens[a], "printsep");
                a = a+1;
            }
            else{
                if (isalpha(token[0])){ 
                    data[a] = ID;
                    strcpy(tokens[a], token);
                    a = a+1;
                }
                else{
                    data[a] = DIGIT;
                    strcpy(tokens[a], token);
                    a = a+1;
                }
            }
        }
        
        for(int k=a; k < N; k++){
            data[k] = 0;   //clears the remainder of the previous round at the start of each lines
        }
        

        if (data[0] == 21){
            continue;    //first token is #
        }

        if (data[0] == 15){ //scalar decleration
            if (data[1] == 24){
                char* currname;
                currname = (char*) malloc(80);  //if you don't do this, the name changes as the tokens[1] changes on each line
                strcpy(currname, tokens[1]);
                
                variables[variableind].name = currname;
                variables[variableind].type = "scalar";
                scalars[scalarind].name = currname;
				fprintf(fptr, "double %s;\n",scalars[scalarind].name);   //decleration for generated file

                variableind++;
                scalarind++;
            }
            else{   // no variable initialization 
                printf("Error (Line %d)\n", linenum ); 
                remove(filename);
                fclose(fptr);
                fclose(fp);
            }
        }

        if (data[0] == 16){ // vector
            if (data[1] == 24){ //name,  identifier
                if (data[2] == 8){
                    if (data[3] == 25){ //size
                        if (data[4] == 9){
                        
                       
                            char* currname;
                            currname = (char*) malloc(80);
                            strcpy(currname, tokens[1]);
                            
                            //We assign a value to the struct in the variables and matrices arrays
                            
                            variables[variableind].name = currname;
                            variables[variableind].type = "vector";
                            matrices[matrixind].name = currname;
                            
                            char* curre_cols;
                            curre_cols = (char*) malloc(80);
                            strcpy(curre_cols,tokens[3]);
                            
                            matrices[matrixind].e_rows = 1; 
                            matrices[matrixind].e_cols = atoi(curre_cols);    //vector so, every column has one element
                            matrices[matrixind].size = atoi(curre_cols);
                            
                            variableind++;
                            matrixind++;
                        }
                        else{
                            printf("Error (Line %d)\n", linenum );  
						    remove(filename);
						    fclose(fptr);
						    fclose(fp);
                        }
                    }
                    else{
                        printf("Error (Line %d)\n", linenum );  
				        remove(filename);
				        fclose(fptr);
				        fclose(fp);
                    }
                }
                else{
                    printf("Error (Line %d)\n", linenum ); 
		            remove(filename);
		            fclose(fptr);
		            fclose(fp);
                }
            }
            else{
                printf("Error (Line %d)\n", linenum ); 
                remove(filename);
                fclose(fptr);
                fclose(fp);
            }
        }
        else if (data[0] == 17){    //matrix
            if (data[1] == 24){ //name
                if (data[2] == 8){
                    if (data[3] == 25){ //e_cols which means row number
                        if (data[4] == 14){
                            if (data[5] == 25){ //e_rows which means column number
                                if (data[6] == 9){
                                    char* currname;
                                    currname = (char*) malloc(80);
                                    strcpy(currname, tokens[1]);
                                    
                                    //We assign a value to the struct in the variables and matrices arrays
                                    
                                    variables[variableind].name = currname;
                                    variables[variableind].type = "matrix";
                                    matrices[matrixind].name = currname;
                                    
                                    char* curre_cols;
                                    curre_cols = (char*) malloc(80);
                                    strcpy(curre_cols,tokens[3]);
                                    
                                    char* curre_rows;
                                    curre_rows = (char*) malloc(80);
                                    strcpy(curre_rows,tokens[5]);
                                    
                                    matrices[matrixind].e_rows = atoi(curre_rows);
                                    matrices[matrixind].e_cols = atoi(curre_cols);
                                    matrices[matrixind].size = atoi(curre_rows)*atoi(curre_cols);
                                    
                                   
                                  
                                    variableind++;
                                    matrixind++;
                                }
                                else{
                                    printf("Error (Line %d)\n", linenum );  
									remove(filename);
									fclose(fptr);
									fclose(fp);
                                }
                            }
                            else{
                                printf("Error (Line %d)\n", linenum );  
								remove(filename);
								fclose(fptr);
								fclose(fp);
                            }
                        }
                        else{
                            printf("Error (Line %d)\n", linenum );  
							remove(filename);
							fclose(fptr);
							fclose(fp);
                        }
                    }
                    else{
                        printf("Error (Line %d)\n", linenum ); 
						remove(filename);
						fclose(fptr);
						fclose(fp);
                    }
                }
                else{
                    printf("Error (Line %d)\n", linenum );  
					remove(filename);
					fclose(fptr);
					fclose(fp);
                }
            }
            else{
                printf("Error (Line %d)\n", linenum );  
				remove(filename);
				fclose(fptr);
				fclose(fp);
            }
        }
        else if (data[0] == 23){   //this means printsep function
            if (data[1] == 3){
                if (data[2] == 4){
                    fprintf(fptr, "printf(\"------------\");\n%s\n", satiratlama);
                }
                else{
                    printf("Error (Line %d)\n", linenum );  
					remove(filename);
					fclose(fptr);
					fclose(fp);
                }
            }
            else{
                printf("Error (Line %d)\n", linenum );  
				remove(filename);
				fclose(fptr);
				fclose(fp);
            }
        }
        
        else if (data[0] == 22){    
            if (data[1] == 3){
                if (data[2] == 24){ //id
                    if(data[3] == 4){
                        
		                char* currname;
		                currname = (char*) malloc(80);
		                strcpy(currname, tokens[2]);
                        for(int ege = 0; ege < sizeof(variables)/sizeof(variables[0]); ege++){
                        	char* name2 = (char*) variables[ege].name;
                        		if (name2 != (NULL) ) {
		                        	if (strcmp(name2, currname) == 0){ 
				                        if(variables[ege].type == "scalar"){  //here we write the code to print the value of the scalar to the generated file
				                            for(int beg = 0; beg < sizeof(scalars)/sizeof(scalars[0]); beg++){
				                            	char* name3 = (char*) scalars[beg].name;
				                            	if (name3 != (NULL) ) {
						                            if (strcmp(name3, currname) == 0){
						                            	fprintf(fptr, "if (is_int(%s)){\n\tint y = %s;\n\tprintf(\"%s\",y);\n\t%s\n}\nelse{\n\tprintf(\"%s\",%s);\n\t%s\n}\n",
						                            	currname,currname, yuzded, satiratlama ,yuzdef,currname, satiratlama);
						                            }
						                        }
				                            }
				                        }
				                        else if(variables[ege].type == "vector"){ //here we write the code to print the value of the every element in a vector to the generated file
				                        	for(int beg = 0; beg < sizeof(matrices)/sizeof(matrices[0]); beg++){
				                            	char* name3 = (char*) matrices[beg].name;
				                            	if (name3 != (NULL) ) {
						                            if (strcmp(name3, currname) == 0){
						                            	fprintf(fptr, "for(int y = 0; y < %d; y++){\n\tif (is_int(%s[y])){\n\tint abw = %s[y];\n\tprintf(\"%s\",abw);\n\t%s\n}\n\telse{\n\tprintf(\"%s\",%s[y]);\n\t%s\n}\n}\n", matrices[beg].size, currname, currname, yuzded, satiratlama, yuzdef, currname, satiratlama);
						                            }
						                        }
				                            }
				                        }
				                        else if(variables[ege].type == "matrix"){ //here we write the code to print the value of the every element in a matrix to the generated file
				                        	for(int beg = 0; beg < sizeof(matrices)/sizeof(matrices[0]); beg++){
				                            	char* name3 = (char*) matrices[beg].name;
				                            	if (name3 != (NULL) ) {
						                            if (strcmp(name3, currname) == 0){
						                            	fprintf(fptr, "rowindex = 0; \nfor(int y = 0; y < %d; y++){ \n\trowindex++;\nif (is_int(%s[y])){\n\tint abw = %s[y];\n\tprintf(\"%s \",abw);\n}\n\telse{\n\tprintf(\"%s \",%s[y]);\n\t}\n if((rowindex %s %d)==0){\n\t%s\n}\n}\n", matrices[beg].size, currname, currname, yuzded, yuzdef, currname, mod, matrices[beg].e_rows, satiratlama);
						                            }
						                        }
				                            }
				                        }
		                        	}
                        		}
                        }
                    }
                    else if(data[3] == 8){
                        if(data[4] == 25){ //num
                            if(data[5] == 9){
                                if(data[6] == 4){
                                    
                                    char* currname;
								    currname = (char*) malloc(80);
								    strcpy(currname, tokens[2]);
								    
								    char* currnum;
								    currnum = (char*) malloc(80);
								    strcpy(currnum, tokens[4]);
								    
								    int mynum = atoi(currnum);
								  
								    
								    for(int f = 0; f < sizeof(matrices)/sizeof(matrices[0]); f++){
						            	char* name2 = (char*) variables[f].name;
						            	if (name2 != (NULL) ) {
						            		if (strcmp(name2, currname) == 0){
						            		
								        		if(variables[f].type == "vector"){ //here we write the code to print the value of the specified element in a vector to the generated file
								                	for(int beg = 0; beg < sizeof(matrices)/sizeof(matrices[0]); beg++){
								                    	char* name3 = (char*) matrices[beg].name;
								                    	if (name3 != (NULL) ) {
										                    if (strcmp(name3, currname) == 0){
										                    	fprintf(fptr, "if (is_int(%s[%d])){\n\tint canhoca = %s[%d];\n\tprintf(\"%s\",canhoca);\n\t%s\n}\nelse{\n\tprintf(\"%s\",%s[%d]);\n\t%s\n}\n",
						                            	currname, mynum-1, currname, mynum-1, yuzded, satiratlama ,yuzdef, currname, mynum-1, satiratlama);
										                    }
										                }
								                    }
						                    	}
						            		}
						            	}
						            }
                                }
                                else{
                                    printf("Error (Line %d)\n", linenum ); 
									remove(filename);
									fclose(fptr);
									fclose(fp);
                                }
                            }
                            else if(data[5] == 14){
                                if(data[6] == 25){
                                    if(data[7] == 9){
                                        if(data[8] == 4){
                                            
                                            char* currname;
											currname = (char*) malloc(80);
											strcpy(currname, tokens[2]);
											
											char* currnum;
											currnum = (char*) malloc(80);
											strcpy(currnum, tokens[4]);
											
											char* currnum2;
											currnum2 = (char*) malloc(80);
											strcpy(currnum2, tokens[6]);
											
											int coor1 = atoi(currnum);
											int coor2 = atoi(currnum2);
											
											//here we write the code to print the value of the specified element in a matrix to the generated file
											
											for(int f = 0; f < sizeof(matrices)/sizeof(matrices[0]); f++){
										    	char* name2 = (char*) matrices[f].name;
										    	if (name2 != (NULL) ) {
										    		if (strcmp(name2, currname) == 0){
										    			int index = ((coor1*(matrices[f].e_rows)) - (matrices[f].e_rows - coor2)-1); 
										    			fprintf(fptr, "if (is_int(%s[%d])){\n\tint canhoca = %s[%d];\n\tprintf(\"%s\",canhoca);\n\t%s\n}\nelse{\n\tprintf(\"%s\",%s[%d]);\n\t%s\n}\n",
						                            	currname, index, currname, index, yuzded, satiratlama ,yuzdef, currname, index, satiratlama);
										    		}
										    	}
										    }
                                        }
                                        else{
                                            printf("Error (Line %d)\n", linenum ); 
											remove(filename);
											fclose(fptr);
											fclose(fp);
                                        }
                                    }
                                    else{
                                        printf("Error (Line %d)\n", linenum ); 
										remove(filename);
										fclose(fptr);
										fclose(fp);
                                    }
                                }
                                else{
                                    printf("Error (Line %d)\n", linenum );  
									remove(filename);
									fclose(fptr);
									fclose(fp);
                                }
                            }
                            else{
                                printf("Error (Line %d)\n", linenum );  
								remove(filename);
								fclose(fptr);
								fclose(fp);
                            }
                        }
                        else{
                                printf("Error (Line %d)\n", linenum );  
								remove(filename);
								fclose(fptr);
								fclose(fp);
                        }
                    }
                    else{
                        printf("Error (Line %d)\n", linenum ); 
						remove(filename);
						fclose(fptr);
						fclose(fp);
                    }
                }
                else{
                    printf("Error (Line %d)\n", linenum );  
					remove(filename);
					fclose(fptr);
					fclose(fp);
                }
            }
            else{
                printf("Error (Line %d)\n", linenum ); 
				remove(filename);
				fclose(fptr);
				fclose(fp);
            }
        }
        else if(data[0] == 24){ //if a line starts with identifier it is an assignment
            if (data[1] == 12){
                if (data[2] == 10){ // assignment with { for vectors and matrices
                    double vectorvalues[N];
                    int b = 2;
                    int c = 0;
                    int size = 0;
                    
                    char* vectorname;
                    vectorname = (char*) malloc(80);
                    strcpy(vectorname, tokens[0]);
                    
                    for(int ty = 0; ty < sizeof(matrices)/sizeof(matrices[0]); ty++){  //We look at the matrices and if the name fits, we get the size of it.
                    	char* name2 = (char*) matrices[ty].name;
                        if(matrices[ty].name != (NULL)){
                        	 if (strcmp(name2,vectorname) == 0) {
                        	 	size = matrices[ty].size;
                        	 	matrices[ty].defined = 1;
                            }
                        }
                    }
                    
                    
                    fprintf(fptr, "double %s[%d] = {", vectorname, size); //vector and matrix declaration and definition
                    
                    while(data[b+1] == 25){
                        char *ptr;
                        double ret;
                        
                        char* currname;
                        currname = (char*) malloc(80);
                        strcpy(currname, tokens[b+1]);
                        
                        vectorvalues[c] = ret = strtod(currname, &ptr);  //we wrote the elements of the vector to the generated file
                        b++;
                        c++;
                        
                        if(data[b+1] == 11){
                        	fprintf(fptr, "%f};\n", ret);
                        }
                        else{
                        	fprintf(fptr, "%f,", ret);
                        }
                    }
                    
                    char* currname;
                    currname = (char*) malloc(80);
                    strcpy(currname, tokens[0]);
                    
                    for(int ty = 0; ty < sizeof(matrices)/sizeof(matrices[0]); ty++){
                    	char* name2 = (char*) matrices[ty].name;
                        if(matrices[ty].name != (NULL)){
                        	 if (strcmp(name2,currname) == 0) {
                             	assignMatrix(vectorvalues, &matrices[ty], matrices[ty].size);  //we cerate a matrix with our values
                            }
                        }
                    }
                    if(data[b+1] != 11){
                    	printf("Error (Line %d)\n", linenum );  
						remove(filename);
						fclose(fptr);
						fclose(fp);
                    }
                    
                }
                else if(data[2] == 25){  //expression which starts with a number
                    if(data[3] == 0){ //The value of our variable is assigned with a single number, we find it simply without inserting it into the expression function
                        int errorval = 1;
                        char* currname;
                        currname = (char*) malloc(80);
                        strcpy(currname, tokens[0]);
                        
                        for(int k = 0; k < sizeof(scalars)/sizeof(scalars[0]); k++){
                            char* name2 = (char*) scalars[k].name;
                            if (scalars[k].name != (NULL) ) {
                                if ( strcmp(name2,currname) == 0) {
                                    char *curr_value;
                                    curr_value = (char *) malloc(80);
                                    strcpy(curr_value, tokens[2]);
                                    scalars[k].d_value = atoi(curr_value);
                                    fprintf(fptr, "%s = %f;\n", currname, scalars[k].d_value);  //scalar definition, we assign our value to the variable
                                    errorval = 0;
                                }
                            }
                        }
                        
                        if (errorval == 1){
                            printf("Error (Line %d)\n", linenum );  
							remove(filename);
							fclose(fptr);
							fclose(fp);
                        }
                    }
                    else{ //expression that starts with a number but contains other variables like scalars, matrices
				        
                    	for(int j = 2; j < a; j++){
		                	char* currname;
		                    currname = (char*) malloc(80);
		                    strcpy(currname, tokens[j]);
		                    
		                    strcpy(expressions[k], currname); 
		                    k++;
		                    
                    	}
				        expr(str); //We send the string with our expression to the expr function, here it becomes a postfix
				        
				        char *string = str;
				        char *q;
				        char *token;
				        int id = 0;
				        
				        q = strdup(string);
				        strstrip(q);
				        
				        while ( (token = strsep(&q, " ")) != NULL){  //We moved the expression in postfix state to char array because we will find value from this array with functions
						    strcpy(exprarr[id],token);
						   	id++;
				        }


                        if (whatres(exprarr)){
                            editscalar(exprarr);
                            for (int beg = 0; beg < sizeof(scalars) / sizeof(scalars[0]); beg++) {
                                char *name3 = (char *) scalars[beg].name;
                                if (name3 != (NULL)) {
                                    if (strcmp(name3, tokens[0]) == 0) {
                                        scalars[beg].d_value = resultt(exprarr); //We find the value of the expression from the function and
                                                                                // assign the value to our variable and print it to the generated file.
                                        fprintf(fptr, "%s = %f;\n", scalars[beg].name, scalars[beg].d_value);
                                    }
                                }
                            }
                        }
                        else {
                            editscalar(exprarr);
                            double adj[200];
                            int ct1 = 0;
                            for (int i = 0; i< sizeof(adj)/sizeof (adj[0]);i++){
                                if (adj[i] != 0.000000){
                                    ct1++;
                                }
                            }
                            resultarr(adj, exprarr);
                            
                            if(error){
                            	remove(filename);
								fclose(fptr);
								fclose(fp);
                            }
                            
                            for (int i = 0;  i< sizeof(matrices)/sizeof(matrices[0]); i++ ){
                                char *name3 = (char *) matrices[i].name;
                                if (name3 != (NULL)) {
                                    if (strcmp(name3, tokens[0]) == 0) {
                                        assignMatrix(adj,&matrices[i],ct1);
                                        if(matrices[i].defined != 1){
                                        	fprintf(fptr, "double %s[%d];\n", matrices[i].name, matrices[i].e_cols*matrices[i].e_rows);
                                        	for(int j = 0; j < matrices[i].e_cols*matrices[i].e_rows; j++){
                                        		fprintf(fptr, "%s[%d] = %f;\n",matrices[i].name, j, matrices[i].value[j]);
                                        	}
                                        }
                                    }
                                }
                            }
                        }


                        //resultt(exprarr);
				        
				        k = 0;
				        memset(expressions, '\0', sizeof(expressions[0][0]) * MAXTOKENS * TOKENLENGTH); //we clear the array for the next line
				        
                    }
                }
                else {   //expression that does not start with number
                	for(int j = 2; j < a; j++){
		                	char* currname;
		                    currname = (char*) malloc(80);
		                    strcpy(currname, tokens[j]);
		                    strcpy(expressions[k], currname);
		                    k++;
		                    
                    	}
				        expr(str);  //We send the string with our expression to the expr function, here it becomes a postfix
				        
				        
				        char *string = str;
				        char *q;
				        char *token;
				        int id = 0;
				        
				        q = strdup(string);
				        strstrip(q);
				        
				        while ( (token = strsep(&q, " ")) != NULL){  //We moved the expression in postfix state to char array because we will find value from this array with functions
				        	strcpy(exprarr[id],token);                   
				        	id++;
				        }

                    if (whatres(exprarr)){
                        editscalar(exprarr);
                        for (int beg = 0; beg < sizeof(scalars) / sizeof(scalars[0]); beg++) {
                            char *name3 = (char *) scalars[beg].name;
                            if (name3 != (NULL)) {
                                if (strcmp(name3, tokens[0]) == 0) {
                                    scalars[beg].d_value = resultt(exprarr); //We find the value of the expression from the function and
                                                                                // assign the value to our variable and print it to the generated file.
                                    fprintf(fptr, "%s = %f;\n", scalars[beg].name, scalars[beg].d_value);
                                }
                            }
                        }
                    }
                        else {
                            editscalar(exprarr);
                            double adj[200];
                            int ct1 = 0;
                            for (int i = 0; i< sizeof(adj)/sizeof (adj[0]);i++){
                                if (adj[i] != 0.000000){
                                    ct1++;
                                }
                            }
                            
                            
                            resultarr(adj, exprarr);
                            
                            if(error){
                            	remove(filename);
								fclose(fptr);
								fclose(fp);
                            }
                            
                            
                            for (int i = 0;  i< sizeof(matrices)/sizeof(matrices[0]); i++ ){
                                char *name3 = (char *) matrices[i].name;
                                if (name3 != (NULL)) {
                                    if (strcmp(name3, tokens[0]) == 0) {
                                        assignMatrix(adj,&matrices[i],ct1);
                                        if(matrices[i].defined != 1){
                                        	fprintf(fptr, "double %s[%d];\n", matrices[i].name, matrices[i].e_cols*matrices[i].e_rows);
                                        	for(int j = 0; j < matrices[i].e_cols*matrices[i].e_rows; j++){
                                        		fprintf(fptr, "%s[%d] = %f;\n",matrices[i].name, j, matrices[i].value[j]);
                                        	}
                                        }
                                    }
                                }
                            }
                        }                    


                    k = 0;
				    memset(expressions, '\0', sizeof(expressions[0][0]) * MAXTOKENS * TOKENLENGTH); //we clear the array for the next line
                }

            }
            
            else{
                printf("Error (Line %d)\n", linenum ); 
				remove(filename);
				fclose(fptr);
				fclose(fp);
            }
        }
        else if (data[0] == 1){
            if (data[1] == 3){
                if (data[2] == 24){   //variable name
                    if(data[3] == 2){  //in 
                    //one loop
                        if (data[4] == 25){  //start indexi
                            if(data[5] == 13){
                                if(data[6] == 25 || data[6] == 24){ //bound value
                                    if(data[7] == 13){
                                        if(data[8] == 25){ //increment value
                                            if(data[9] == 4){
                                                if(data[10] == 10){
                                                	
                                                	int finishidx;
                                                	
                                                	char* varname;
													varname = (char*) malloc(80);
													strcpy(varname, tokens[2]);
		                    
													char* startidx;
													startidx = (char*) malloc(80);
													strcpy(startidx, tokens[4]);
		                    
													char* finishidxx;
													finishidxx = (char*) malloc(80);
													strcpy(finishidxx, tokens[6]);
													
													char* increment;
													increment = (char*) malloc(80);
													strcpy(increment, tokens[8]);
													
													forgiris = 1;
													
													if(data[6] == 24){
														for(int k = 0; k < sizeof(scalars)/sizeof(scalars[0]); k++){
															char* name2 = (char*) scalars[k].name;
															if (scalars[k].name != (NULL) ) {
																if ( strcmp(name2,finishidxx) == 0) {
																    finishidx = scalars[k].d_value;
																}
															}
                        								}
														
													}
													else{
														finishidx = atoi(finishidx);
													}
													forsayisi = finishidx;
													fprintf(fptr, "for (%s = %d; %s < %d; %s += %d){\n", varname, atoi(startidx), varname, finishidx, varname, atoi(increment));
													//for line for generated file
													
													
                                                }
                                                else{
		                                            printf("Error (Line %d)\n", linenum ); 
													remove(filename);
													fclose(fptr);
													fclose(fp);
                                                }
                                            }
                                            else{
                                            	printf("Error (Line %d)\n", linenum ); 
												remove(filename);
												fclose(fptr);
												fclose(fp);
                                            }
                                        }
                                        else{
                                        	printf("Error (Line %d)\n", linenum ); 
											remove(filename);
											fclose(fptr);
											fclose(fp);
                                        }
                                    }
                                    else{
                                    
                                    	printf("Error (Line %d)\n", linenum ); 
										remove(filename);
										fclose(fptr);
										fclose(fp);
                                    }
                                }
                                else{
                                
                                        printf("a\n");
                                	printf("Error (Line %d)\n", linenum ); 
									remove(filename);
									fclose(fptr);
									fclose(fp);
                                }
                            }
                            else{
                            	printf("Error (Line %d)\n", linenum ); 
								remove(filename);
								fclose(fptr);
								fclose(fp);
                            }
                        }
                        else{
                        	printf("Error (Line %d)\n", linenum ); 
							remove(filename);
							fclose(fptr);
							fclose(fp);
                        }
                    }
                    else if(data[3] == 14){
                    //double loop
                    }
                    else{
                    	printf("Error (Line %d)\n", linenum ); 
						remove(filename);
						fclose(fptr);
						fclose(fp);
                    }
                }
                else{
                	printf("Error (Line %d)\n", linenum ); 
					remove(filename);
					fclose(fptr);
					fclose(fp);
                }
            }
            else{
            	printf("Error (Line %d)\n", linenum ); 
				remove(filename);
				fclose(fptr);
				fclose(fp);
            }
        }
        else if (data[0] == 11){  //that means for is closed
        	forgiris = 0;
        	fprintf(fptr, "}\n");
        }
        cur = 0;
        memset(tokens, '\0', sizeof(tokens[0][0]) * MAXTOKENS * TOKENLENGTH);


    }
	fprintf(fptr, "return(0);\n");
	fprintf(fptr, "}");
    fclose(fp);
    return(0);

}




