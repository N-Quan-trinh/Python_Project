#include <stdio.h>
#include <stdlib.h>
#include "dirent.h"
#include "string.h"


struct Node{
    char *name;
    struct Node *next;
};


void insertAfter(struct Node* prev_node, char* new_data);

void alp(struct Node* x[26]);

char *strsplt(const char* ranstr, int x);

void printList(struct Node* n, char* sample);

void insertionSort(char* arr[], int n);


int main(){

    int x = 1;
    char path[100];
    printf("Please enter your Directory");
    scanf("%[ -/0-9a-z:-@A-Z[-`]", path);
    char *prefix = NULL;
    prefix = (char *) malloc(sizeof(char*));
    if(prefix == NULL){
        printf("can't create memory for prefix!");
        return 0;
    }
    while ((getchar()) != '\n');
    while(x) {
        fflush(stdin);
        printf("Enter a prefix\n");
        fgets(prefix, 7, stdin);
        if(prefix[0] == '\n'){
            x = 0;
        }
        // create an Array of 26 elements, with each having the head's value of an alphabet

        struct Node *list[26];
        for (int item = 0; item < 26; item++) {
            struct Node *first = (struct Node *) malloc(sizeof(struct Node));
            first->name = ".";
            first->next = NULL;
            list[item] = first;
        }
        alp(list);


        //Getting the entry of given directory/ sorting
        int file_count = 0; //This will be used for the for loop, iterating through the filnames
        DIR *d;
        struct dirent *dir;
        char *filenames[225];//A pointer points to string of filenames, each having a {MAX_NUMBER} 0f 225.
        int i = 0;
        d = opendir(path);
        if (d) {
            while ((dir = readdir(d)) != NULL) {

                filenames[i] = malloc(strlen(dir->d_name) + 1);
                strcpy(filenames[i], dir->d_name);
                i++;
                file_count++;

            }
            closedir(d);
        }
        insertionSort(filenames, file_count);
        //Using the Filename's indexes, i will insertAfter each entry into fitting linked List.
        for (int index = 0; index < file_count; index++) {
            if (filenames[index][0] > 64 && filenames[index][0] < 92) {

                insertAfter(list[filenames[index][0] - 65], filenames[index]);
            } else if (filenames[index][0] > 96 && filenames[index][0] < 123) {

                insertAfter(list[filenames[index][0] - 97], filenames[index]);
            }
        }


        for(int x2=0; x2<26; x2++){
            if(list[x2]->name[0] == prefix[0]){
                printf("Files starting with %s", prefix);
                printList(list[x2], prefix);
            }
        }
        }
    //end of Sort
    return 0;
}





//Function initializations
void insertAfter(struct Node* prev_node, char* new_data)
{
    /*1. check if the given prev_node is NULL */
    if (prev_node == NULL)
    {
        printf("the given previous node cannot be NULL");
        return;
    }

    /* 2. allocate new node */
    struct Node* new_node =(struct Node*) malloc(sizeof(struct Node));

    /* 3. put in the data */
    new_node->name = new_data;

    /* 4. Make next of new node as next of prev_node */
    new_node->next = prev_node->next;

    /* 5. move the next of prev_node as new_node */
    prev_node->next = new_node;
}

void insertionSort(char* arr[], int n)
{
    int i, j;
    char* key;
    for (i = 1; i < n; i++) {
        key = arr[i];
        j = i - 1;

        /* Move elements of arr[0..i-1], that are
          greater than key, to one position ahead
          of their current position */
        while (j >= 0 && (strcmp(arr[j], key)) == 1) {
            arr[j + 1] = arr[j];
            j = j - 1;
        }
        arr[j + 1] = key;
    }
}

void printList(struct Node* n, char* sample)
{
    while (n != NULL) {

        if(strcmp(strsplt(n->name, strlen(sample)-1), sample) == 0) {
            printf("%s\n", n->name);
        }
        n = n->next;
    }
}

char *strsplt(const char* ranstr, int x){
    char* re = (char*) malloc(sizeof(char*));
    for(int e=0; e<x; e++){
        re[e] = ranstr[e];
    }
    re[x] = '\n';
    re[x+1] = '\0';
    return re;
}


void alp(struct Node *x[26]) {
    x[0]->name = "A";
    x[1]->name = "B";
    x[2]->name = "C";
    x[3]->name = "D";
    x[4]->name = "E";
    x[5]->name = "F";
    x[6]->name = "G";
    x[7]->name = "H";
    x[8]->name = "I";
    x[9]->name = "J";
    x[10]->name = "K";
    x[11]->name = "L";
    x[12]->name = "M";
    x[13]->name = "N";
    x[14]->name = "O";
    x[15]->name = "P";
    x[16]->name = "Q";
    x[17]->name = "R";
    x[18]->name = "S";
    x[19]->name = "T";
    x[20]->name = "U";
    x[21]->name = "V";
    x[22]->name = "W";
    x[23]->name = "X";
    x[24]->name = "Y";
    x[25]->name = "Z";
}
//End of code