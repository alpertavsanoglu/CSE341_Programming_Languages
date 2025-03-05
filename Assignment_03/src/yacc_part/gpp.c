#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gpp.h"

void init_table(SymbolTable *table)
{
    table->size = 0;
}

void define_function(SymbolTable *table, char *id, int num_args, func_data data/*Valuef (*func2)(Valuef, Valuef)*/)
{
    if (table->size == MAX_TABLE_SIZE)
    {
        printf("Symbol table is full\n");
        exit(1);
    }
    table->entries[table->size].id = strdup(id);
    table->entries[table->size].num_args = num_args;
    table->entries[table->size].data.func = data.func;
    ++table->size;
}

func_data use_function(SymbolTable *table, char *id, int num_args, func_data arg1, func_data arg2)
{
    int i;
    for (i = 0; i < table->size; ++i)
    {
        if (strcmp(table->entries[i].id, id) == 0){
            return table->entries[i].data.func(arg1, arg2);
        }
    }
    printf("Function %s not found\n", id);
    exit(1);
}

void free_table(SymbolTable *table)
{
    int i;
    for (i = 0; i < table->size; ++i)
        free(table->entries[i].id);
}

func_data valuef_add(func_data v1, func_data v2) {
    func_data v;
    v.value.num  = v1.value.num * v2.value.denom + v2.value.num * v1.value.denom;
    v.value.denom = v1.value.denom * v2.value.denom;
    return v;
}

func_data valuef_sub(func_data v1, func_data v2) {
    func_data v;
    v.value.num = v1.value.num * v2.value.denom - v2.value.num * v1.value.denom;
    v.value.denom = v1.value.denom * v2.value.denom;
    return v;
}

func_data valuef_mult(func_data v1, func_data v2) {
    func_data v;
    v.value.num = v1.value.num * v2.value.num;
    v.value.denom = v1.value.denom * v2.value.denom;
    return v;
}

func_data valuef_div(func_data v1, func_data v2) {
    func_data v;
    v.value.num = v1.value.num * v2.value.denom;
    v.value.denom = v1.value.denom * v2.value.num;
    return v;
}

Valuef valuef_convert(char * str) {
    Valuef v;
    v.num=0;
    v.denom=0;
    int i = 0;
    while (str[i] != 'f') {
        v.num = (v.num * 10) + (str[i] - '0');
        i++;
    }
    i++;
    while (str[i] != '\0') {
        v.denom = (v.denom * 10) + (str[i] - '0');
        i++;
    }
    return v;
}

func_data valuef_create(int num, int denom) {
    func_data v;
    v.value.num = num;
    v.value.denom = denom;
    return v;
}
