/* Simple hex0 compiler - minimal hex to binary converter for bootstrapping */
#include <stdio.h>
#include <stdlib.h>

int hex_to_val(char c) {
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'A' && c <= 'F') return c - 'A' + 10;
    if (c >= 'a' && c <= 'f') return c - 'a' + 10;
    return -1;
}

int main(int argc, char* argv[]) {
    if (argc != 3) {
        fprintf(stderr, "Usage: %s input.hex0 output.bin\n", argv[0]);
        return 1;
    }
    
    FILE* in = fopen(argv[1], "r");
    FILE* out = fopen(argv[2], "wb");
    if (!in || !out) {
        fprintf(stderr, "Error opening files\n");
        return 1;
    }
    
    int c, toggle = 0, store = 0;
    while ((c = fgetc(in)) != EOF) {
        if (c == '#' || c == ';') {
            // Skip comments to end of line
            while ((c = fgetc(in)) != EOF && c != '\n');
            continue;
        }
        
        int val = hex_to_val(c);
        if (val == -1) continue; // Skip non-hex chars
        
        if (toggle) {
            fputc(store | val, out);
            toggle = 0;
        } else {
            store = val << 4;
            toggle = 1;
        }
    }
    
    fclose(in);
    fclose(out);
    return 0;
}