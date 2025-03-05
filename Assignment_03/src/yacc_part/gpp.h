#ifndef GPP_H
#define GPP_H
#define MAX_TABLE_SIZE 99

typedef struct Valuef {
    int num;    /* numerator */
    int denom;  /* denominator */
} Valuef;

typedef struct func_data{
    struct func_data (*func)(struct func_data, struct func_data);
    Valuef value;
} func_data;

typedef struct function
{
    char *id;
    int num_args;
    func_data data;
} function;


typedef struct SymbolTable
{
    int size;
    function entries[MAX_TABLE_SIZE];
} SymbolTable;

void init_table(SymbolTable *table);

void define_function(SymbolTable *table, char *id, int num_args, func_data data/*Valuef (*func2)(Valuef, Valuef)*/);

func_data use_function(SymbolTable *table, char *id, int num_args, func_data arg1, func_data arg2);

void free_table(SymbolTable *table);

func_data valuef_add(func_data v1, func_data v2);

func_data valuef_sub(func_data v1, func_data v2);

func_data valuef_mult(func_data v1, func_data v2);

func_data valuef_div(func_data v1, func_data v2);

Valuef valuef_convert(char * str);

func_data valuef_create(int num, int denom);

#endif
