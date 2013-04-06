//
// This program computes the checksum for nxp cortex-m devices as explained here:
// http://sigalrm.blogspot.com/2011/10/cortex-m3-exception-vector-checksum.html
// http://support.code-red-tech.com/CodeRedWiki/OutputFormats
//
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

int main(int argc, char* argv[]) {

	if(argc < 2) {
		printf("Usage: %s filename.bin\n", argv[0]);
		exit(1);
	}

	FILE *binfile;

	char *filename = argv[1];

	binfile = fopen(filename, "r+");

	if(binfile != NULL) {

		uint32_t checksum = 0;

		for(uint8_t index = 0; index < 7; index++) {

			uint32_t word;

			if(fread(&word, 4, 1, binfile) != 1) {
				printf("Error reading from file\n");
				exit(1);
			}

			checksum += word;
		}

		checksum = -checksum;

		fseek(binfile, 0x1c, SEEK_SET);
		fwrite(&checksum, 4, 1, binfile);
		fclose(binfile);

	} else {
		printf("Error opening file %s\n", filename);
	}

	return 0;
}
