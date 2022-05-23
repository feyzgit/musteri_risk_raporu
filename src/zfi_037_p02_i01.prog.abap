

DATA: go_app TYPE REF TO zfi_037_cl02.
TABLES : bsid, knvv, knb4, bkpf, bseg, t001,kna1, lfa1, rfpdo , but000 .

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001 .
 PARAMETERS    :   p_keydt like sy-datum OBLIGATORY DEFAULT sy-datum .
 SELECT-OPTIONS:   s_bukrs FOR t001-bukrs ,
                   s_part FOR but000-partner .



SELECTION-SCREEN END OF BLOCK b1 .
*
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002 .
*
*
  PARAMETERS: p_guncel AS CHECKBOX DEFAULT 'X' .
SELECTION-SCREEN END OF BLOCK b2 .
