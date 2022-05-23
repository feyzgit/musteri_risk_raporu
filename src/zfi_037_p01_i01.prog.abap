*&---------------------------------------------------------------------*
*& Include          ZFI_070_P01_I01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Include          ZFI_059_P01_I01
*&---------------------------------------------------------------------*

DATA: go_app TYPE REF TO zfi_037_cl01.
TABLES : bsid, knvv, knb4, bkpf, bseg, t001,kna1, lfa1, rfpdo , but000 .

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001 .
  PARAMETERS: r_up RADIOBUTTON GROUP r1,
              r_bp RADIOBUTTON GROUP r1,
              r_ap RADIOBUTTON GROUP r1 .

SELECTION-SCREEN END OF BLOCK b1 .

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002 .
  PARAMETERS: p_keydt like bkpf-budat OBLIGATORY DEFAULT sy-datum.
  SELECT-OPTIONS : s_bukrs FOR t001-bukrs ,

                   s_part FOR but000-partner ,
                   s_vkorg FOR knvv-vkorg ,
                   s_vtweg FOR knvv-vtweg  ,
                   s_kdgrp FOR knvv-kdgrp ,
                   s_vkgrp FOR knvv-vkgrp ,
                   s_vkbur FOR knvv-vkbur .

  PARAMETERS: p_konzs AS CHECKBOX DEFAULT '' .
  PARAMETERS: p_vergi as CHECKBOX DEFAULT '' .
 SELECTION-SCREEN END OF BLOCK b2 .

*SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003 .
**  PARAMETERS: p_kosul AS CHECKBOX DEFAULT '' .
*
*SELECTION-SCREEN END OF BLOCK b3 .
