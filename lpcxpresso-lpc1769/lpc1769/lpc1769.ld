/* From: http://ulan.git.sourceforge.net/git/gitweb.cgi?p=ulan/sysless;a=blob;f=board/arm/lpc17xx-common/libs/ldscripts/lpc1768.ld-cfg;h=c37ec2de11b5b3d9667a89257c4ec3acecb09b64;hb=HEAD */

PROVIDE( __bbconf_pt_addr = 0 );

MEMORY
{
        /* LPC176[89] : 512 ROM + 32k SRAM + 16k SRAM + 16k SRAM */
        /*--------------------------------------------------- */


        /* Main ROM region - 512k for LPC176[89] */
        FLASH (rx) : ORIGIN = 0x00000000, LENGTH = 512k

        /* local static RAM - 32k for LPC176[89] */
        IRAM0 (rwx) : ORIGIN = 0x10000000, LENGTH = 32k

        /* stack location */
        STACK (rw) : ORIGIN = 0x10000000 + 0x00007FE0 - 4, LENGTH = 4

        /* AHB SRAM - 2x16k for LPC176[89] - often used for USB */
        USBRAM (rwx) : ORIGIN = 0x2007C000, LENGTH = 4k
        IRAM1  (rwx) : ORIGIN = 0x2007D000, LENGTH = 28k
}

/* From: http://ulan.git.sourceforge.net/git/gitweb.cgi?p=ulan/sysless;a=blob;f=board/arm/lpc17xx-common/libs/ldscripts/lpc17xx-base.ld-boot;h=b2c64fa863fba493dfc23aa3a6604851b73e1770;hb=HEAD */

/***********************************************************************/
/*                                                                     */
/*  ROM.ld:  Linker Script File                                        */
/*                                                                     */
/***********************************************************************/

ENTRY(g_pfnVectors)

/* SECTION command : Define mapping of input sections */
/* into output sections. */

SECTIONS
{
	/******************************************/
	/* code section */

        /* "normal" code */
	
	.text :
	{
		KEEP(*(.isr_vector .isr_vector.*))
		*(.text .text.*)
		*(.gnu.linkonce.t.*)
		*(.glue_7)
		*(.glue_7t)
		*(.gcc_except_table)
		*(.rodata .rodata*)
		*(.gnu.linkonce.r.*)
	} >FLASH

	/******************************************/
	/* .ctors .dtors are used for c++ constructors/destructors */
	.ctors :
	{
		. = ALIGN(4);
		PROVIDE(__ctors_start = .);
		KEEP(*(SORT(.ctors.*)))
		KEEP(*(.ctors))
		PROVIDE(__ctors_end = .);
	} >FLASH

	.dtors :
	{
		. = ALIGN(4);
		PROVIDE(__dtors_start = .);
		KEEP(*(SORT(.dtors.*)))
		KEEP(*(.dtors))
		PROVIDE(__dtors_end = .);

		. = ALIGN(4);
		/* End Of .text section */
		_etext = .;
                _sifastcode = .;
	} >FLASH
	
        .irqarea (NOLOAD):
        {
                . = ALIGN (256);
                *(.irqarea .irqarea.*)

        } >IRAM0

	/**************************************************/
        /* fastcode - copied at startup & executed in RAM */

        .fastcode :
        {
          . = ALIGN (4);
          _sfastcode = . ;

          *(.glue_7t) *(.glue_7)
          *(.fastcode)
          
          /* add other modules here ... */

          . = ALIGN (4);
          _efastcode = . ;
          _sidata = .;
        } >IRAM0 AT>FLASH

        /******************************************/
	/* data section */
	.data :
  	{
        	_sidata = LOADADDR (.data);
  		. = ALIGN(4);
		_sdata = .;

		*(vtable vtable.*)
		*(.data .data.*)
		*(.gnu.linkonce.d*)

		. = ALIGN(4);
		_edata = . ;
	} >IRAM0 AT>FLASH

	/******************************************/
	/* For no-init variables section */
	.bss (NOLOAD) :
	{
		. = ALIGN(4);
		_sbss = . ;
		__bss_start__ = . ;

		*(.bss .bss.*)
		*(.gnu.linkonce.b*)
		*(COMMON)

		. = ALIGN(4);
		_ebss = . ;
		__bss_end__ = .;

		. = ALIGN(4);
		_end = . ;
		PROVIDE (end = .);
	} >IRAM0

        .stack :
        {
             _stack = .;
        } >STACK

	/******************************************/
	/* This used for USB RAM section */
	.usbram (NOLOAD):
	{
		_usbram = . ;
		USB_RAM_ADR = . ;
		*(.usbram)
		. = ALIGN(4);
		_eusbram = . ;
		_usbram_end = . ;
	} > USBRAM

	/* Extra RAM */
	.iram1 (NOLOAD):
	{
		_iram = . ;
		*(.iram1)
		. = ALIGN(4);
		_iram_end = . ;
	} > IRAM1

	/******************************************/
	/* Stabs debugging sections.  */
	.stab          0 : { *(.stab) }
	.stabstr       0 : { *(.stabstr) }
	.stab.excl     0 : { *(.stab.excl) }
	.stab.exclstr  0 : { *(.stab.exclstr) }
	.stab.index    0 : { *(.stab.index) }
	.stab.indexstr 0 : { *(.stab.indexstr) }
	/* .comment       0 : { *(.comment) } */
	/* DWARF debug sections.
	 Symbols in the DWARF debugging sections are relative to the beginning
	 of the section so we begin them at 0.  */
	/* DWARF 1 */
	.debug          0 : { *(.debug) }
	.line           0 : { *(.line) }
	/* GNU DWARF 1 extensions */
	.debug_srcinfo  0 : { *(.debug_srcinfo) }
	.debug_sfnames  0 : { *(.debug_sfnames) }
	/* DWARF 1.1 and DWARF 2 */
	.debug_aranges  0 : { *(.debug_aranges) }
	.debug_pubnames 0 : { *(.debug_pubnames) }
	/* DWARF 2 */
	.debug_info     0 : { *(.debug_info .gnu.linkonce.wi.*) }
	.debug_abbrev   0 : { *(.debug_abbrev) }
	.debug_line     0 : { *(.debug_line) }
	.debug_frame    0 : { *(.debug_frame) }
	.debug_str      0 : { *(.debug_str) }
	.debug_loc      0 : { *(.debug_loc) }
	.debug_macinfo  0 : { *(.debug_macinfo) }
	/* SGI/MIPS DWARF 2 extensions */
	.debug_weaknames 0 : { *(.debug_weaknames) }
	.debug_funcnames 0 : { *(.debug_funcnames) }
	.debug_typenames 0 : { *(.debug_typenames) }
	.debug_varnames  0 : { *(.debug_varnames) }
}
