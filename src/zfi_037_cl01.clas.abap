class ZFI_037_CL01 definition
  public
  final
  create public .

public section.

  interfaces ZBC_000_IF01 .

  aliases GO_ALV
    for ZBC_000_IF01~GO_ALV .
  aliases GO_LOG
    for ZBC_000_IF01~GO_LOG .
  aliases GT_SEL_ROWS
    for ZBC_000_IF01~GT_SEL_ROWS .
  aliases GV_BAL_LOG_DISPLAY
    for ZBC_000_IF01~GV_BAL_LOG_DISPLAY .
  aliases GV_HANDLE
    for ZBC_000_IF01~GV_HANDLE .
  aliases GV_KEEP_ROWS
    for ZBC_000_IF01~GV_KEEP_ROWS .
  aliases GV_STRUC_NAME
    for ZBC_000_IF01~GV_STRUC_NAME .
  aliases GV_TITLE
    for ZBC_000_IF01~GV_TITLE .
  aliases ALV_BUTTON_CLICK
    for ZBC_000_IF01~ALV_BUTTON_CLICK .
  aliases ALV_DOUBLE_CLICK
    for ZBC_000_IF01~ALV_DOUBLE_CLICK .
  aliases ALV_HOTSPOT_CLICK
    for ZBC_000_IF01~ALV_HOTSPOT_CLICK .
  aliases ALV_MENU_BUTTON
    for ZBC_000_IF01~ALV_MENU_BUTTON .
  aliases ALV_TOOLBAR
    for ZBC_000_IF01~ALV_TOOLBAR .
  aliases ALV_USER_COMMAND
    for ZBC_000_IF01~ALV_USER_COMMAND .
  aliases AT_SELECTION_SCREEN
    for ZBC_000_IF01~AT_SELECTION_SCREEN .
  aliases AT_SELECTION_SCREEN_OUTPUT
    for ZBC_000_IF01~AT_SELECTION_SCREEN_OUTPUT .
  aliases CREATE_ALV_OBJECT
    for ZBC_000_IF01~CREATE_ALV_OBJECT .
  aliases END_OF_SELECTION
    for ZBC_000_IF01~END_OF_SELECTION .
  aliases EXCLUDE_BUTTONS
    for ZBC_000_IF01~EXCLUDE_BUTTONS .
  aliases FILL_FIELD_CATALOG
    for ZBC_000_IF01~FILL_FIELD_CATALOG .
  aliases FILL_LAYOUT
    for ZBC_000_IF01~FILL_LAYOUT .
  aliases GET_DATAS
    for ZBC_000_IF01~GET_DATAS .
  aliases SCREEN_PAI
    for ZBC_000_IF01~SCREEN_PAI .
  aliases SCREEN_PBO
    for ZBC_000_IF01~SCREEN_PBO .
  aliases START_OF_SELECTION
    for ZBC_000_IF01~START_OF_SELECTION .
  aliases USER_COMMAND
    for ZBC_000_IF01~USER_COMMAND .

  types:
    BEGIN OF ty_range,
        p_bukrs TYPE zfi_036_p01_s01-p_bukrs,
        p_kunnr TYPE zfi_036_p01_s01-p_kunnr,
        s_gjahr TYPE zfi_036_p01_s01-s_gjahr,

      END OF ty_range .

  data MS_RANGE type ZFI_037_S01 .
  data MT_ALV type ZFI_037_S02_TT01 .
  data MT_ALV2 type ZFI_037_S05_TT01 .
  data MO_GRID type ref to CL_GUI_ALV_GRID .
  data MV_ERROR type CHAR1 .
  data:
    it_bdcdata    TYPE TABLE OF bdcdata .
  data:
    gs_bdcdata    LIKE LINE OF it_bdcdata .
  data:
    it_bdcmsgcoll TYPE TABLE OF bdcmsgcoll .
  data GS_BDCMSGCOLL type BDCMSGCOLL .
  data MV_SUBMIT type XFELD value '' ##NO_TEXT.
  data MV_SD type XFELD value '' ##NO_TEXT.
  data MV_VERGI type XFELD value '' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      value(IV_VERGI) type XFELD optional .
  class-methods GET_INSTANCE
    returning
      value(RO_RESULT) type ref to ZFI_037_CL01 .
  methods INITIALIZATION
    changing
      !CV_VARIANT type DISVARIANT-VARIANT .
  methods TOP_OF_PAGE
    changing
      value(CO_DYNDOC_ID) type ref to CL_DD_DOCUMENT optional .
  methods SELECT_DATA .
  methods UP .
  methods BP .
  methods AP .
  methods CHECK_AUTHORIZATION .
protected section.
private section.

  class-data MO_SINGLETON type ref to ZFI_037_CL01 .
  data MV_REPORT type RALDB_REPO value 'ZFI_037_P01' ##NO_TEXT.
  data MV_ANY_CLR_DOC type XFELD .
ENDCLASS.



CLASS ZFI_037_CL01 IMPLEMENTATION.


  METHOD ZBC_000_IF01~AT_SELECTION_SCREEN_OUTPUT.

*-&Read Selection Parameter->
    zbc_000_cl01=>get_selections_from_program(
      EXPORTING
        iv_prog = CONV #( me->mv_report )
      IMPORTING
        es_range = me->ms_range ) .


  ENDMETHOD.


  METHOD ZBC_000_IF01~SCREEN_PAI.

    CASE iv_ucomm.
      WHEN ZFI_000_CL02=>MC_IV_UCOMM_BACK.
        LEAVE TO SCREEN 0.
      WHEN ZFI_000_CL02=>MC_IV_UCOMM_EXIT OR ZFI_000_CL02=>MC_IV_UCOMM_CANCEL.
        LEAVE SCREEN.
    ENDCASE.

  ENDMETHOD.


  METHOD ZBC_000_IF01~CREATE_ALV_OBJECT.


    DATA : lo_container TYPE REF TO cl_gui_custom_container.
    DATA : ls_layout  TYPE lvc_s_layo,
           ls_variant TYPE disvariant.
    DATA : lt_exclude TYPE ui_functions,
           lt_fcat    TYPE lvc_t_fcat.
*-----

    CREATE OBJECT lo_container
      EXPORTING
        container_name = ZFI_000_CL02=>mc_container_name_alvcontainer.

    CREATE OBJECT zbc_000_if01~go_alv
      EXPORTING
        i_parent = lo_container.

    lt_fcat    = me->zbc_000_if01~fill_field_catalog( ).

    lt_exclude = me->zbc_000_if01~exclude_buttons( ).
    ls_layout  = me->zbc_000_if01~fill_layout( ).

    ls_variant-report  = sy-cprog.
*    ls_variant-variant = me->gs_range-p_vari.

*    SET HANDLER me->zbc_000_if01~alv_button_click  FOR zbc_000_if01~go_alv.
    SET HANDLER me->zbc_000_if01~alv_double_click  FOR zbc_000_if01~go_alv.
*    SET HANDLER me->zbc_000_if01~alv_menu_button   FOR zbc_000_if01~go_alv.
    SET HANDLER me->zbc_000_if01~alv_toolbar       FOR zbc_000_if01~go_alv.
    SET HANDLER me->zbc_000_if01~alv_user_command  FOR zbc_000_if01~go_alv.
*    SET HANDLER me->zbc_000_if01~alv_hotspot_click FOR zbc_000_if01~go_alv.
*    SET HANDLER me->data_changed FOR zbc_000_if01~go_alv.

    CALL METHOD zbc_000_if01~go_alv->set_table_for_first_display
      EXPORTING
        is_layout            = ls_layout
        it_toolbar_excluding = lt_exclude
        is_variant           = ls_variant
        i_save               = zfi_000_cl02=>mc_i_save_u
        i_default            = abap_true
      CHANGING
        it_outtab            = mt_alv
        it_fieldcatalog      = lt_fcat.

    zbc_000_if01~go_alv->register_edit_event( cl_gui_alv_grid=>mc_evt_enter ).
    zbc_000_if01~go_alv->register_edit_event( cl_gui_alv_grid=>mc_evt_modified ).


  ENDMETHOD.


  METHOD zbc_000_if01~alv_double_click.

    READ TABLE me->mt_alv INTO DATA(ls_alv) INDEX e_row .

    IF sy-subrc IS INITIAL .


      SET PARAMETER ID zfi_000_cl02=>mc_paramid_buk FIELD ls_alv-bukrs .
      SET PARAMETER ID zfi_000_cl02=>mc_id_kun FIELD ls_alv-kunnr .
      SET PARAMETER ID zfi_000_cl02=>mc_paramid_gjr FIELD ms_range-p_keydt(4).
      CALL TRANSACTION zfi_000_cl02=>mc_tcode_fd10n AND SKIP FIRST SCREEN .

    ENDIF .

  ENDMETHOD.


  method ZBC_000_IF01~ALV_DATA_CHANGED.
  endmethod.


  METHOD ap.


*----------------------------------------*
*     DATA DEFINATIONS
*----------------------------------------*

    DATA: ls_alv LIKE LINE OF mt_alv.
    DATA: lv_tabix TYPE sy-tabix.
    DATA: var_soll TYPE knc1-umsav.
    DATA: var_haben TYPE knc1-umsav.
    DATA: wa_knc1 TYPE knc1 .
    DATA: wa_knb4 TYPE knb4 .
    DATA: tarih TYPE kaljahr.
    DATA: jahxx TYPE kaljahr.
    DATA: var_ag  TYPE knb4-agn01.
    DATA: var_vz  TYPE knb4-vzn01.
    DATA: ct      TYPE i.
    DATA: prod(8) TYPE p.
    DATA: toplam    TYPE fdbl_de_bal,  "agsxx,
          tutar_bpb TYPE fdbl_de_bal,  "agsxx,
          tutar_upb TYPE fdbl_de_bal.

    DATA: lt_fatura TYPE TABLE OF bsid.

    DATA : lr_cl62 TYPE REF TO zfi_062_cl02 .
    DATA : ls_62_s05 TYPE zfi_062_s05 .
    DATA : lt_62_all TYPE zfi_062_s06_tt01 .
    DATA : r_augdt TYPE zbc_000_tt02 .
    DATA : s_augdt TYPE LINE OF zbc_000_tt02.
    TYPES : BEGIN OF ty_bukrs,
              bukrs TYPE bukrs,
            END OF ty_bukrs,
            tt_bukrs TYPE STANDARD TABLE OF ty_bukrs WITH DEFAULT KEY.

*----------------------------------------*
*     ANAVERİ
*----------------------------------------*

    SELECT a~kunnr,
           a~land1,
           a~name1,
           a~stcd2,
           a~regio,
           b~bukrs,
           a~konzs,
           c~vkorg,
           c~vtweg,
           c~vkbur,
           c~vkgrp,
           c~kdgrp,
           c~zterm,
           e~bezei,
           f~vtext,
           b~kultg,
           c~waers

    FROM kna1 AS a
         INNER JOIN knb1 AS b ON b~kunnr EQ a~kunnr
         LEFT JOIN knvv AS c  ON c~kunnr EQ b~kunnr
         LEFT JOIN tvko AS d  ON d~vkorg EQ c~vkorg
                             AND d~bukrs EQ b~bukrs
         LEFT JOIN t005u AS e ON a~land1 EQ e~land1
                             AND a~regio EQ e~bland
                             AND e~spras EQ @sy-langu
         LEFT JOIN tvtwt AS f ON c~vtweg EQ f~vtweg
                             AND f~spras EQ @sy-langu
    WHERE a~kunnr IN @ms_range-s_part AND
          b~bukrs IN @ms_range-s_bukrs AND
          b~bukrs NE @space AND
          c~vkorg IN @ms_range-s_vkorg AND
          c~vkgrp IN @ms_range-s_vkgrp AND
          c~vkbur IN @ms_range-s_vkbur AND
          c~kdgrp IN @ms_range-s_kdgrp AND
          c~vtweg IN @ms_range-s_vtweg AND
           ( ( c~vkorg IS NULL AND d~vkorg IS NULL ) OR
             ( b~bukrs EQ d~bukrs ) )
             ORDER BY f~vtweg
    INTO TABLE @DATA(lt_kna1)
    .

    SELECT * FROM t052
      INTO TABLE @DATA(lt_052) .

    "" her durumda aynı vkn'li müşterileri getirsin
*    IF ms_range-p_vergi IS NOT INITIAL OR
*           mv_vergi IS NOT INITIAL.
    " aynı vergi no da olan cariler çekilecek
    " r_part dolacak , alttaki selectte de rpart kullanılacak

    CHECK lt_kna1 IS NOT INITIAL.

    SELECT a~kunnr, a~land1, a~name1, a~stcd2,
           a~regio, b~bukrs, a~konzs, c~vkorg,
           c~vtweg, c~vkbur, c~vkgrp, c~kdgrp,
           c~zterm, e~bezei, f~vtext, b~kultg,
           c~waers
    FROM kna1 AS a
         INNER JOIN knb1 AS b ON b~kunnr EQ a~kunnr
         LEFT JOIN knvv AS c  ON c~kunnr EQ b~kunnr
         LEFT JOIN tvko AS d  ON d~vkorg EQ c~vkorg
                             AND d~bukrs EQ b~bukrs
         LEFT JOIN t005u AS e ON a~land1 EQ e~land1
                             AND a~regio EQ e~bland
                             AND e~spras EQ @sy-langu
         LEFT JOIN tvtwt AS f ON c~vtweg EQ f~vtweg
                             AND f~spras EQ @sy-langu
      FOR ALL ENTRIES IN @lt_kna1
    WHERE a~stcd2 EQ @lt_kna1-stcd2 AND
          a~kunnr NE @lt_kna1-kunnr AND
          a~stcd2 NE '' AND
          b~bukrs IN @ms_range-s_bukrs AND
          b~bukrs NE @space AND
          c~vkorg IN @ms_range-s_vkorg AND
          c~vkgrp IN @ms_range-s_vkgrp AND
          c~vkbur IN @ms_range-s_vkbur AND
          c~kdgrp IN @ms_range-s_kdgrp AND
          c~vtweg IN @ms_range-s_vtweg AND
           ( ( c~vkorg IS NULL AND d~vkorg IS NULL ) OR
             ( b~bukrs EQ d~bukrs ) )
*             ORDER BY f~vtweg
    APPENDING TABLE @lt_kna1 .
    SORT lt_kna1 BY kunnr vtweg .
    DELETE ADJACENT DUPLICATES FROM lt_kna1 .
    SORT lt_kna1 BY vtweg kunnr .
    DELETE ADJACENT DUPLICATES FROM lt_kna1 COMPARING vtweg kunnr .
*    ENDIF.
*----------------------------------------*
*     AÇIK KALEMLER
*----------------------------------------*
    IF lt_kna1 IS NOT INITIAL.

      SELECT * FROM bsid INTO TABLE @DATA(lt_bsid)
        FOR ALL ENTRIES IN @lt_kna1
      WHERE bukrs EQ @lt_kna1-bukrs AND
            kunnr EQ @lt_kna1-kunnr AND
            budat LE @ms_range-p_keydt .

      SELECT * FROM bsad APPENDING TABLE lt_bsid
               FOR ALL ENTRIES IN lt_kna1
      WHERE bukrs EQ lt_kna1-bukrs AND
            kunnr EQ lt_kna1-kunnr AND
               budat LE ms_range-p_keydt AND
               augdt GT ms_range-p_keydt .
    ENDIF.
*----------------------------------------*
*     ÇEK DURUMU
*----------------------------------------*
    IF lt_bsid IS NOT INITIAL.

      SELECT * FROM bsed INTO TABLE @DATA(lt_bsed)
        FOR ALL ENTRIES IN @lt_bsid
        WHERE bukrs EQ @lt_bsid-bukrs AND
              belnr EQ @lt_bsid-belnr AND
              gjahr EQ @lt_bsid-gjahr AND
              buzei EQ @lt_bsid-buzei .

    ENDIF.
*----------------------------------------*
*     DBS alanları
*----------------------------------------*
    IF lt_kna1 IS NOT INITIAL.
      SELECT * FROM zfi_089_t03 INTO TABLE @DATA(lt_dbs)
        FOR ALL ENTRIES IN @lt_kna1
              WHERE bukrs EQ @lt_kna1-bukrs AND
                  kunnr   EQ @lt_kna1-kunnr .

    ENDIF.


*----------------------------------------*
*     ORGANİZASYONEL DATA
*----------------------------------------*
    " lt_but000 tablosu hiç bir yerde kullanılmadığı için kapatılmıştır.
* selectte alan belirtilmediği için ATC Checkde takılıyor.
*    SELECT * FROM but000 INTO TABLE @DATA(lt_but000)
*      WHERE partner IN @ms_range-s_part.

    SELECT * FROM t001  INTO TABLE @DATA(lt_001)
      WHERE bukrs IN @ms_range-s_bukrs.

*    IF lt_bsid IS INITIAL."atc checkte kullanılmadığı için hata veriyor kapatıldı XD_FATIHK |10.05.2021
*      SELECT * FROM bkpf INTO TABLE @DATA(lt_bkpf)
*        FOR ALL ENTRIES IN @lt_bsid
*        WHERE bukrs EQ @lt_bsid-bukrs AND
*              belnr EQ @lt_bsid-belnr AND
*              gjahr EQ @lt_bsid-gjahr.
*    ENDIF.


*----------------------------------------*
*     SİPARİŞ VE TESLİMAT
*----------------------------------------*

*    SELECT
*       tvko~bukrs ,
*       vbak~vkorg,
*       vbak~kunnr,
*       vbap~vbeln,
*       vbap~posnr,
*       vbak~lifsk,
*       vbak~waerk,
*       vbap~kwmeng,
*       vbap~netwr,
*       vbap~mwsbp,
*       vbap~lfsta
*    INTO TABLE @DATA(lt_vbap)
*    FROM vbak AS vbak INNER JOIN vbap AS vbap
*    ON vbak~vbeln EQ vbap~vbeln
*    INNER JOIN tvko AS tvko
*    ON vbak~vkorg EQ tvko~vkorg
*      FOR ALL ENTRIES IN @lt_kna1
*    WHERE vbak~kunnr EQ @lt_kna1-kunnr
*    AND tvko~bukrs IN @ms_range-s_bukrs
*    AND vbak~vbtyp EQ @zfi_000_cl02=>mc_vbtyp_c
*    AND vbap~absta NE @zfi_000_cl02=>mc_absta_c
*    AND vbap~gbsta NE @zfi_000_cl02=>mc_gbsta_c
*    AND vbap~lfsta IN (@zfi_000_cl02=>mc_lfsta_a,@zfi_000_cl02=>mc_lfsta_b).


    SELECT lips~vbeln,
           lips~posnr,
           lips~lfimg,
           likp~wbstk,
           likp~kunag,
           vbap~netwr,
           vbap~mwsbp,
           vbap~kwmeng,
           vbap~waerk,
           tvko~bukrs,
           likp~vkorg,
           lips~vtweg
    INTO  TABLE @DATA(lt_lips)
    FROM likp AS likp
    INNER JOIN lips AS lips
    ON likp~vbeln EQ lips~vbeln
    INNER JOIN vbap AS vbap
    ON lips~vgbel EQ vbap~vbeln AND
    lips~vgpos EQ vbap~posnr
    INNER JOIN tvko AS tvko
    ON likp~vkorg EQ tvko~vkorg
      FOR ALL ENTRIES IN @lt_kna1
    WHERE tvko~bukrs IN @ms_range-s_bukrs
    AND likp~kunag EQ @lt_kna1-kunnr
    AND likp~fkstk IN (@zfi_000_cl02=>mc_fkstk_a, @zfi_000_cl02=>mc_fkstk_b)
    AND likp~gbstk IN (@zfi_000_cl02=>mc_gbstk_a,@zfi_000_cl02=>mc_gbstk_b)
    AND likp~vbtyp EQ @zfi_000_cl02=>mc_vbtyp_j.

*----------------------------------------*
*     MÜŞTERİ KOŞUL TABLSO
*----------------------------------------*



    SELECT * FROM zfi_009_t01 INTO TABLE @DATA(lt_09)
      FOR ALL ENTRIES IN @lt_kna1
      WHERE
      bukrs IN @ms_range-s_bukrs AND
      kunnr EQ  @lt_kna1-kunnr.
    IF lt_kna1 IS NOT INITIAL.

      SELECT * FROM zfi_206_t01 INTO TABLE @DATA(lt_206)
        FOR ALL ENTRIES IN @lt_kna1
        WHERE kunnr EQ @lt_kna1-kunnr AND
              sevk_bit_tar GE @ms_range-p_keydt AND
              sevk_bas_tar LE @ms_range-p_keydt.
    ENDIF.
    SORT lt_206 BY sevk_bit_tar DESCENDING.

*----------------------------------------*
********     PROCESS DATA   *************
*----------------------------------------*

    LOOP AT lt_kna1 REFERENCE INTO DATA(lr_kna1).

      IF lr_kna1->waers IS INITIAL.
        READ TABLE lt_001 INTO DATA(ls_001)
        WITH KEY bukrs = lr_kna1->bukrs.
        IF sy-subrc IS INITIAL.
          lr_kna1->waers = ls_001-waers.
        ENDIF.
      ENDIF.

    ENDLOOP.

    LOOP AT lt_bsid REFERENCE INTO DATA(lr_bsid).
*
      CLEAR: ls_alv.

      READ TABLE lt_kna1 INTO  DATA(ls_kna1)
      WITH KEY kunnr = lr_bsid->kunnr.

      IF sy-subrc IS INITIAL.
        ls_alv-waers = ls_kna1-waers.

        CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
          EXPORTING
*           CLIENT           = SY-MANDT
            date             = ms_range-p_keydt
            foreign_amount   = lr_bsid->wrbtr
            foreign_currency = lr_bsid->waers
            local_currency   = ls_alv-waers
*           RATE             = 0
            type_of_rate     = zfi_000_cl02=>mc_type_of_rate_m
            read_tcurr       = abap_true
          IMPORTING
*           EXCHANGE_RATE    =
*           FOREIGN_FACTOR   =
            local_amount     = lr_bsid->dmbtr
*           LOCAL_FACTOR     =
*           EXCHANGE_RATEX   =
*           FIXED_RATE       =
*           DERIVED_RATE_TYPE       =
          EXCEPTIONS
            no_rate_found    = 1
            overflow         = 2
            no_factors_found = 3
            no_spread_found  = 4
            derived_2_times  = 5
            OTHERS           = 6.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.


      ENDIF.
*
      IF lr_bsid->shkzg EQ zfi_000_cl02=>mc_shkzg_h.
        MULTIPLY lr_bsid->dmbtr BY -1 .
        MULTIPLY lr_bsid->wrbtr BY -1 .
        MULTIPLY lr_bsid->dmbe2 BY -1 .
        MULTIPLY lr_bsid->dmbe3 BY -1 .
      ENDIF.
      " vade tarihi
      CALL FUNCTION 'NET_DUE_DATE_GET'
        EXPORTING
          i_zfbdt = lr_bsid->zfbdt
          i_zbd1t = lr_bsid->zbd1t
          i_zbd2t = lr_bsid->zbd2t
          i_zbd3t = lr_bsid->zbd3t
          i_shkzg = lr_bsid->shkzg
          i_rebzg = lr_bsid->rebzg
          i_koart = zfi_000_cl02=>mc_koart_d
        IMPORTING
          e_faedt = lr_bsid->zfbdt.

      ls_alv-bukrs = lr_bsid->bukrs.
      ls_alv-kunnr = lr_bsid->kunnr.

      IF ms_range-p_konzs IS NOT INITIAL.

        READ TABLE lt_kna1 REFERENCE INTO lr_kna1
        WITH KEY kunnr = lr_bsid->kunnr.

        IF sy-subrc IS INITIAL.
          IF  lr_kna1->konzs IS NOT INITIAL.
            ls_alv-konzs = lr_kna1->konzs .
          ENDIF.
        ENDIF.

      ENDIF.

*----------------------------------------------------*
*                     CH_BAKIYE
*----------------------------------------------------*

      IF lr_bsid->umskz EQ space.
        ADD lr_bsid->dmbtr TO ls_alv-ch_bakiye.
      ENDIF.


*----------------------------------------------------*
*                     ÇEKLER
*----------------------------------------------------*
      READ TABLE lt_bsed REFERENCE INTO DATA(lr_bsed)
      WITH KEY bukrs = lr_bsid->bukrs
               belnr = lr_bsid->belnr
               gjahr = lr_bsid->gjahr
               buzei = lr_bsid->buzei.

      IF sy-subrc IS INITIAL AND
        ( lr_bsid->umskz EQ zfi_000_cl02=>mc_umskz_1 OR
        lr_bsid->umskz EQ zfi_000_cl02=>mc_umskz_s ) .
        ADD lr_bsid->dmbtr TO ls_alv-c_toplam.

        IF lr_bsed->wstat = zfi_000_cl02=>mc_wstat_m.
          ADD lr_bsid->dmbtr TO ls_alv-vgkct.
          ADD lr_bsid->dmbtr TO ls_alv-vgckt.
        ELSE.
          ADD lr_bsid->dmbtr TO ls_alv-vgcckt.
          ADD lr_bsid->dmbtr TO ls_alv-vgckt.

          IF lr_bsid->zfbdt GE ms_range-p_keydt.
            ADD lr_bsid->dmbtr TO ls_alv-vgcckt2.
          ENDIF.

        ENDIF.


      ENDIF.

*----------------------------------------------------*
*                     EXIM E YÜKLÜ FATURA
*----------------------------------------------------*

      IF lr_bsid->xref3 EQ zfi_000_cl02=>mc_xref3_exim.

        ADD lr_bsid->dmbtr TO ls_alv-eli_teminat.

      ENDIF.

*----------------------------------------------------*
*                     TEMİNATLAR
*----------------------------------------------------*

      IF lr_bsid->umskz EQ zfi_000_cl02=>mc_umskz_tem_mek AND
         lr_bsid->zfbdt GE ms_range-p_keydt..
        SUBTRACT lr_bsid->dmbtr FROM ls_alv-tem_mek.
      ENDIF.

      IF lr_bsid->umskz EQ zfi_000_cl02=>mc_umskz_ipotek AND
         lr_bsid->zfbdt GE ms_range-p_keydt..
        SUBTRACT lr_bsid->dmbtr FROM ls_alv-ipotek.
      ENDIF.

      IF lr_bsid->umskz EQ zfi_000_cl02=>mc_umskz_exim AND
         lr_bsid->zfbdt GE ms_range-p_keydt..
        SUBTRACT lr_bsid->dmbtr FROM ls_alv-exim.
      ENDIF.

      IF lr_bsid->umskz EQ zfi_000_cl02=>MC_UMSKZ_AKREDITIF AND
         lr_bsid->zfbdt GE ms_range-p_keydt.
        SUBTRACT lr_bsid->dmbtr FROM ls_alv-akreditif.
      ENDIF.

*----------------------------------------------------*
*                     UPB & PB
*----------------------------------------------------*

*      READ TABLE lt_001 REFERENCE INTO DATA(lr_001)
*      WITH KEY bukrs = lr_bsid->bukrs.
*      IF sy-subrc IS INITIAL.
*        ls_alv-waers = lr_001->waers.
*      ENDIF.


      COLLECT ls_alv INTO mt_alv.

    ENDLOOP.

*----------------------------------------------------*
*          zfi_009 tablosunda tanımlı cari
*----------------------------------------------------*

    LOOP AT lt_09 INTO DATA(lsx_09).

      CLEAR ls_alv.
      READ TABLE mt_alv INTO ls_alv WITH KEY bukrs = lsx_09-bukrs
                                             kunnr = lsx_09-kunnr.

      IF sy-subrc IS NOT INITIAL.
        CLEAR ls_alv.
        "MOVE-CORRESPONDING lsx_09 to
        ls_alv-bukrs = lsx_09-bukrs.
        ls_alv-kunnr = lsx_09-kunnr.

        READ TABLE lt_kna1 REFERENCE INTO lr_kna1
        with key kunnr = lsx_09-kunnr
                 bukrs = lsx_09-bukrs.

        if sy-subrc is INITIAL.
        ls_alv-waers = lr_kna1->waers .
        endif.

        APPEND ls_alv TO mt_alv .

      ENDIF.

    ENDLOOP.


    IF mv_sd NE abap_true .

*----------------------------------------------------*
*                   TASİLAT SÜRESİ
*----------------------------------------------------*
      s_augdt-sign   = 'I'.
      s_augdt-option = 'BT' .
      s_augdt-low    = ms_range-p_keydt .
      s_augdt-low(4)    = ms_range-p_keydt(4) - 1  .
      s_augdt-high   = ms_range-p_keydt .

      APPEND s_augdt TO r_augdt .

      DATA(it_bukrs) = VALUE tt_bukrs( FOR GROUPS grp OF wa IN mt_alv
                     GROUP BY ( bukrs = wa-bukrs )
                              "  waers = wa-waers )
                             ( bukrs = grp-bukrs ) ) .


      LOOP AT it_bukrs INTO DATA(ls_t001) .
        CREATE OBJECT lr_cl62.
        lr_cl62->ms_range-p_bukrs = ls_t001-bukrs .
        lr_cl62->ms_range-s_part = me->ms_range-s_part .
        lr_cl62->ms_range-s_augdt = r_augdt .
        lr_cl62->ms_range-p_erken = abap_true .
        lr_cl62->ms_range-p_tam = abap_true .
        lr_cl62->ms_range-p_gec = abap_true .
        lr_cl62->ms_range-p_iade = abap_true .
        lr_cl62->ms_range-p_odeme = abap_true .
        lr_cl62->mc_submit = abap_true .
        CALL METHOD lr_cl62->get_datas .

        DATA(lt_62_tmp) = lr_cl62->mt_alv .

        LOOP AT lt_62_tmp INTO DATA(ls_62_tmp) .
          APPEND ls_62_tmp TO lt_62_all .
        ENDLOOP .
      ENDLOOP .

    ENDIF.

    LOOP AT mt_alv REFERENCE INTO DATA(lr_alv).

*----------------------------------------------------*
*                     DBS LİMİT ve KDT
*----------------------------------------------------*

      LOOP AT lt_dbs REFERENCE INTO DATA(lr_dbs)
        WHERE kunnr EQ lr_alv->kunnr AND
              bukrs EQ lr_alv->bukrs.
        ADD lr_dbs->dbslm TO lr_alv->dbslm.
        ADD lr_dbs->riskl TO lr_alv->dbs_kredi.

      ENDLOOP.

*----------------------------------------------------*
*                     müşteri ana veri
*----------------------------------------------------*
      READ TABLE lt_kna1 REFERENCE INTO lr_kna1
      WITH KEY kunnr = lr_alv->kunnr
               bukrs = lr_alv->bukrs .

      IF sy-subrc IS INITIAL.
        lr_alv->name1 = lr_kna1->name1 .
        lr_alv->bland = lr_kna1->regio .
        lr_alv->vtweg = lr_kna1->vtweg .
        lr_alv->vkorg = lr_kna1->vkorg .
        lr_alv->bezei = lr_kna1->bezei .
        lr_alv->vtext = lr_kna1->vtext .
        lr_alv->stcd2 = lr_kna1->stcd2 .

        IF lr_kna1->zterm IS NOT INITIAL.
          READ TABLE lt_052 REFERENCE INTO DATA(lr_052)
          WITH KEY zterm = lr_kna1->zterm.
          IF sy-subrc IS INITIAL.

            lr_alv->s_vade = lr_052->ztag1 .

          ENDIF.

          lr_alv->g_vade = + lr_alv->s_vade
                           + lr_kna1->kultg .


        ENDIF.

      ENDIF.

      READ TABLE lt_206 INTO DATA(ls_206)
      WITH KEY kunnr = lr_alv->kunnr
         vkorg = lr_alv->vkorg
         vtweg = lr_alv->vtweg .

      IF sy-subrc IS INITIAL.
        ADD ls_206-ek_vade TO lr_alv->g_vade .
      ENDIF.

*----------------------------------------------------*
*              TESLİMAT TUTARI
*----------------------------------------------------*

      LOOP AT lt_lips REFERENCE INTO DATA(lr_lips)
        WHERE bukrs EQ lr_alv->bukrs AND
              kunag EQ lr_alv->kunnr AND
              vkorg EQ lr_alv->vkorg. " AND
*              vtweg EQ lr_alv->vtweg.

        CHECK lr_lips->kwmeng GT 0.

        CLEAR: tutar_bpb, tutar_upb .
        tutar_bpb = ( lr_lips->netwr  + lr_lips->mwsbp   )
        / lr_lips->kwmeng * lr_lips->lfimg.


        CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
          EXPORTING
*           CLIENT           = SY-MANDT
            date             = ms_range-p_keydt
            foreign_amount   = tutar_bpb
            foreign_currency = lr_lips->waerk
            local_currency   = lr_alv->waers
*           RATE             = 0
            type_of_rate     = zfi_000_cl02=>mc_type_of_rate_m
            read_tcurr       = abap_true
          IMPORTING
*           EXCHANGE_RATE    =
*           FOREIGN_FACTOR   =
            local_amount     = tutar_upb
*           LOCAL_FACTOR     =
*           EXCHANGE_RATEX   =
*           FIXED_RATE       =
*           DERIVED_RATE_TYPE       =
          EXCEPTIONS
            no_rate_found    = 1
            overflow         = 2
            no_factors_found = 3
            no_spread_found  = 4
            derived_2_times  = 5
            OTHERS           = 6.
        IF sy-subrc <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.


        IF lr_lips->wbstk EQ zfi_000_cl02=>mc_wbstk_c.
*          ADD tutar_upb TO lr_alv->t_tutar.
        ELSE.
          ADD tutar_upb TO lr_alv->t_tutar .

        ENDIF.

      ENDLOOP.

**----------------------------------------------------*
**                FORMÜLLER
**----------------------------------------------------*


* toplam risk
      lr_alv->t_risk =  + lr_alv->ch_bakiye
                        + lr_alv->t_tutar
                        + lr_alv->c_toplam .



* toplam teminat exim ve cek dahil
      lr_alv->e_c_t_teminat = + lr_alv->tem_mek
                              + lr_alv->dbslm
                              + lr_alv->ipotek
                              + lr_alv->exim
                              + lr_alv->cek_tem_tutar.

* toplam teminat
      lr_alv->t_teminat =     + lr_alv->tem_mek
                              + lr_alv->dbslm
                              - lr_alv->dbs_kredi
                              + lr_alv->ipotek
                              + lr_alv->exim
                              + lr_alv->akreditif
.

* toplam teminatlı risk
      IF lr_alv->e_c_t_teminat LE lr_alv->t_risk.

        lr_alv->t_tli_risk = lr_alv->e_c_t_teminat.

      ELSE.

        lr_alv->t_tli_risk = lr_alv->t_risk .

      ENDIF.

      IF lr_alv->t_tli_risk LT 0 .
        lr_alv->t_tli_risk = 0 .
      ENDIF.

* toplam teminatsız risk
      lr_alv->tsiz_risk =  + lr_alv->t_risk
                           - lr_alv->t_tli_risk .

      IF lr_alv->tsiz_risk LE 0 .

        lr_alv->tsiz_risk = 0 .

      ENDIF.

* toplam müşteri kredi limiti

      READ TABLE lt_09 INTO DATA(ls_09)
      WITH KEY kunnr = lr_alv->kunnr .
      " peşin ve limitsiz tıkları

      IF sy-subrc IS INITIAL.
        lr_alv->limitsiz_x = ls_09-limitsiz_x.
        lr_alv->pesin_x = ls_09-pesin_x.
      ENDIF.

      DATA(lv_teminat) = + lr_alv->t_teminat
                         - lr_alv->exim.

      lr_alv->toplam = lv_teminat.

      " koşul teminatı
      IF ls_09-teminat_x EQ abap_true .
        IF ( ( lv_teminat ) * ls_09-teminat / 100 ) LE ls_09-max_teminat .
          lr_alv->toplam = + lr_alv->toplam
                           + ( lv_teminat * ls_09-teminat / 100 ) .
          lr_alv->teminat_x = ( lv_teminat * ls_09-teminat / 100 ) .
        ELSE.
          lr_alv->toplam = + lr_alv->toplam
                           + ls_09-max_teminat .
          lr_alv->teminat_x = ls_09-max_teminat .
        ENDIF.

      ENDIF.

      " sabit açık limiti
      IF ls_09-tarih GE ms_range-p_keydt AND
         ls_09-tutar_x EQ abap_true.
        lr_alv->toplam = + lr_alv->toplam
                         + ls_09-tutar .
        lr_alv->sabit_x = ls_09-tutar .
      ENDIF.

      " cirolu çek teminatı
      IF ls_09-cirolu_x EQ abap_true .

        IF ( lr_alv->vgcckt2 * ls_09-cirolu / 100 ) LE ls_09-cirolu_max .
          lr_alv->toplam = + lr_alv->toplam
                           + ( lr_alv->vgcckt2 * ls_09-cirolu / 100 ) .
          lr_alv->cek_tem_tutar = ( lr_alv->vgcckt2 * ls_09-cirolu / 100 ).
        ELSE.
          lr_alv->toplam = + lr_alv->toplam
                           + ls_09-cirolu_max .
          lr_alv->cek_tem_tutar = ls_09-cirolu_max .
        ENDIF.

      ENDIF.

* exim ve çek dahil toplam teminat
      lr_alv->e_c_t_teminat = + lr_alv->tem_mek
                              + lr_alv->dbslm
                              + lr_alv->ipotek
                              + lr_alv->exim
                              + lr_alv->cek_tem_tutar.

      " exim limiti

      IF ls_09-exim_x EQ abap_true .
        ADD lr_alv->exim TO lr_alv->toplam .
      ENDIF.

      " son 1 sene tahsilat süresi

      READ TABLE lt_62_all INTO DATA(ls_62)
      WITH KEY partner = lr_alv->kunnr
               bukrs   = lr_alv->bukrs.

      IF sy-subrc IS INITIAL.

        lr_alv->f_t_vade = ls_62-ogun .
        lr_alv->f_f_vade = ls_62-fgun .

      ENDIF.

      " limit blokajı
      IF lr_alv->toplam LT lr_alv->t_risk.
        lr_alv->limit_b = abap_true.
      ENDIF.

      " Kullanılabilir Limit
      lr_alv->kul_limit = + lr_alv->toplam
                          - lr_alv->t_risk .

    ENDLOOP.

    SORT mt_alv BY  bukrs kunnr .

  ENDMETHOD.


  METHOD bp.


*----------------------------------------*
*     DATA DEFINATIONS
*----------------------------------------*

    DATA: ls_alv LIKE LINE OF mt_alv.
    DATA: lv_tabix TYPE sy-tabix.
    DATA: var_ag  TYPE knb4-agn01.
    DATA: var_vz  TYPE knb4-vzn01.
    DATA: ct      TYPE i.
    DATA: prod(8) TYPE p.
    DATA: toplam           TYPE fdbl_de_bal,  "agsxx,
          tutar_bpb        TYPE fdbl_de_bal,  "agsxx,
          tutar_upb        TYPE fdbl_de_bal,  "agsxx,
          toplam_tum       TYPE fdbl_de_bal,
          vade_asimi       TYPE p DECIMALS 0,  "vzsxx,
          vade_asimi_tum   TYPE p DECIMALS 0, "vzsxx,
          ag_toplam        TYPE fdbl_de_bal,
          alacak_toplam    TYPE fdbl_de_bal,
          ag_alacak_toplam TYPE fdbl_de_bal,
          borc_toplam      TYPE fdbl_de_bal,
          ag_borc_toplam   TYPE fdbl_de_bal,
          ag_toplam_tum    TYPE fdbl_de_bal.

    DATA : lr_cl62 TYPE REF TO zfi_062_cl02 .
    DATA : ls_62_s05 TYPE zfi_062_s05 .
    DATA : lt_62_all TYPE zfi_062_s06_tt01 .
    DATA : r_augdt TYPE zbc_000_tt02 .
    DATA : s_augdt TYPE LINE OF zbc_000_tt02.
    TYPES : BEGIN OF ty_bukrs,
              bukrs TYPE bukrs,
            END OF ty_bukrs,
            tt_bukrs TYPE STANDARD TABLE OF ty_bukrs WITH DEFAULT KEY.

*----------------------------------------*
*     ANAVERİ
*----------------------------------------*

    SELECT a~kunnr,
           a~land1,
           a~name1,
           a~stcd2,
           a~regio,
           b~bukrs,
           a~konzs,
           c~vkorg,
           c~vtweg,
           c~vkbur,
           c~vkgrp,
           c~kdgrp,
           c~zterm,
           e~bezei,
           f~vtext,
           b~kultg

    FROM kna1 AS a
         INNER JOIN knb1 AS b ON b~kunnr EQ a~kunnr
         LEFT JOIN knvv AS c  ON c~kunnr EQ b~kunnr
         LEFT JOIN tvko AS d  ON d~vkorg EQ c~vkorg
                             AND d~bukrs EQ b~bukrs
         LEFT JOIN t005u AS e ON a~land1 EQ e~land1
                             AND a~regio EQ e~bland
                             AND e~spras EQ @sy-langu
         LEFT JOIN tvtwt AS f ON c~vtweg EQ f~vtweg
                             AND f~spras EQ @sy-langu
    WHERE a~kunnr IN @ms_range-s_part AND
          b~bukrs IN @ms_range-s_bukrs AND
          b~bukrs NE @space AND
          c~vkorg IN @ms_range-s_vkorg AND
          c~vkgrp IN @ms_range-s_vkgrp AND
          c~vkbur IN @ms_range-s_vkbur AND
          c~kdgrp IN @ms_range-s_kdgrp AND
          c~vtweg IN @ms_range-s_vtweg AND
           ( ( c~vkorg IS NULL AND d~vkorg IS NULL ) OR
             ( b~bukrs EQ d~bukrs ) )
             ORDER BY f~vtweg
    INTO TABLE @DATA(lt_kna1)
    .

    SELECT * FROM t052
      INTO TABLE @DATA(lt_052) .

    "" her durumda aynı vkn'li müşterileri getirsin
*  IF ms_range-p_vergi IS NOT INITIAL OR
*       mv_vergi IS NOT INITIAL.
    " aynı vergi no da olan cariler çekilecek
    " r_part dolacak , alttaki selectte de rpart kullanılacak
    IF  lt_kna1[] IS NOT INITIAL.
      SELECT a~kunnr, a~land1, a~name1, a~stcd2,
             a~regio, b~bukrs, a~konzs, c~vkorg,
             c~vtweg, c~vkbur, c~vkgrp, c~kdgrp,
             c~zterm, e~bezei, f~vtext, b~kultg
      FROM kna1 AS a
           INNER JOIN knb1 AS b ON b~kunnr EQ a~kunnr
           LEFT JOIN knvv AS c  ON c~kunnr EQ b~kunnr
           LEFT JOIN tvko AS d  ON d~vkorg EQ c~vkorg
                               AND d~bukrs EQ b~bukrs
           LEFT JOIN t005u AS e ON a~land1 EQ e~land1
                               AND a~regio EQ e~bland
                               AND e~spras EQ @sy-langu
           LEFT JOIN tvtwt AS f ON c~vtweg EQ f~vtweg
                               AND f~spras EQ @sy-langu
        FOR ALL ENTRIES IN @lt_kna1
      WHERE a~stcd2 EQ @lt_kna1-stcd2 AND
            a~kunnr NE @lt_kna1-kunnr AND
            a~stcd2 NE '' AND
            b~bukrs IN @ms_range-s_bukrs AND
            b~bukrs NE @space AND
            c~vkorg IN @ms_range-s_vkorg AND
            c~vkgrp IN @ms_range-s_vkgrp AND
            c~vkbur IN @ms_range-s_vkbur AND
            c~kdgrp IN @ms_range-s_kdgrp AND
            c~vtweg IN @ms_range-s_vtweg AND
             ( ( c~vkorg IS NULL AND d~vkorg IS NULL ) OR
               ( b~bukrs EQ d~bukrs ) )
*             ORDER BY f~vtweg
      APPENDING TABLE @lt_kna1 .
    ENDIF.
    SORT lt_kna1 BY kunnr vtweg .
    DELETE ADJACENT DUPLICATES FROM lt_kna1 .
    SORT lt_kna1 BY vtweg kunnr .
    DELETE ADJACENT DUPLICATES FROM lt_kna1 COMPARING vtweg kunnr .
*    ENDIF.

*----------------------------------------*
*     AÇIK KALEMLER
*----------------------------------------*
    IF lt_kna1 IS NOT INITIAL.

      SELECT * FROM bsid INTO TABLE @DATA(lt_bsid)
        FOR ALL ENTRIES IN @lt_kna1
      WHERE bukrs EQ @lt_kna1-bukrs AND
            kunnr EQ @lt_kna1-kunnr AND
            budat LE @ms_range-p_keydt .

      SELECT * FROM bsad APPENDING TABLE lt_bsid
               FOR ALL ENTRIES IN lt_kna1
      WHERE bukrs EQ lt_kna1-bukrs AND
            kunnr EQ lt_kna1-kunnr AND
               budat LE ms_range-p_keydt AND
               augdt GT ms_range-p_keydt .
    ENDIF.
*----------------------------------------*
*     ÇEK DURUMU
*----------------------------------------*
    IF lt_bsid IS NOT INITIAL.

      SELECT * FROM bsed INTO TABLE @DATA(lt_bsed)
        FOR ALL ENTRIES IN @lt_bsid
        WHERE bukrs EQ @lt_bsid-bukrs AND
              belnr EQ @lt_bsid-belnr AND
              gjahr EQ @lt_bsid-gjahr AND
              buzei EQ @lt_bsid-buzei .

    ENDIF.
*----------------------------------------*
*     DBS alanları
*----------------------------------------*
    IF lt_kna1 IS NOT INITIAL.
      SELECT * FROM zfi_089_t03 INTO TABLE @DATA(lt_dbs)
        FOR ALL ENTRIES IN @lt_kna1
              WHERE bukrs EQ @lt_kna1-bukrs AND
                  kunnr   EQ @lt_kna1-kunnr .

    ENDIF.


*----------------------------------------*
*     ORGANİZASYONEL DATA
*----------------------------------------*
    " lt_but000 tablosu hiç bir yerde kullanılmadığı için kapatılmıştır.
* selectte alan belirtilmediği için ATC Checkde takılıyor.
*    SELECT * FROM but000 INTO TABLE @DATA(lt_but000)
*      WHERE partner IN @ms_range-s_part.

    SELECT * FROM t001  INTO TABLE @DATA(lt_001)
      WHERE bukrs IN @ms_range-s_bukrs.

*    IF lt_bsid IS INITIAL."atc checkde kullanılmadıgı için kapatıldı xd_fatihk
*      SELECT * FROM bkpf INTO TABLE @DATA(lt_bkpf)
*        FOR ALL ENTRIES IN @lt_bsid
*        WHERE bukrs EQ @lt_bsid-bukrs AND
*              belnr EQ @lt_bsid-belnr AND
*              gjahr EQ @lt_bsid-gjahr.
*    ENDIF.


*----------------------------------------*
*     SİPARİŞ VE TESLİMAT
*----------------------------------------*

    SELECT
       tvko~bukrs ,
       vbak~vkorg,
       vbak~kunnr,
       vbap~vbeln,
       vbap~posnr,
       vbak~lifsk,
       vbak~waerk,
       vbap~kwmeng,
       vbap~netwr,
       vbap~mwsbp,
       vbap~lfsta
    INTO TABLE @DATA(lt_vbap)
    FROM vbak AS vbak INNER JOIN vbap AS vbap
    ON vbak~vbeln EQ vbap~vbeln
    INNER JOIN tvko AS tvko
    ON vbak~vkorg EQ tvko~vkorg
    WHERE vbak~kunnr IN @ms_range-s_part
    AND tvko~bukrs IN @ms_range-s_bukrs
    AND vbak~vbtyp EQ @zfi_000_cl02=>mc_vbtyp_c
    AND vbap~absta NE @zfi_000_cl02=>mc_absta_c
    AND vbap~gbsta NE @zfi_000_cl02=>mc_gbsta_c
    AND vbap~lfsta IN (@zfi_000_cl02=>mc_lfsta_a,@zfi_000_cl02=>mc_lfsta_b).


    SELECT lips~vbeln,
           lips~posnr,
           lips~lfimg,
           likp~wbstk,
           likp~kunag,
           vbap~netwr,
           vbap~mwsbp,
           vbap~kwmeng,
           vbap~waerk,
           tvko~bukrs,
           likp~vkorg,
           lips~vtweg
    INTO  TABLE @DATA(lt_lips)
    FROM likp AS likp
    INNER JOIN lips AS lips
    ON likp~vbeln EQ lips~vbeln
    INNER JOIN vbap AS vbap
    ON lips~vgbel EQ vbap~vbeln AND
    lips~vgpos EQ vbap~posnr
    INNER JOIN tvko AS tvko
    ON likp~vkorg EQ tvko~vkorg
    WHERE tvko~bukrs IN @ms_range-s_bukrs
    AND likp~kunag IN @ms_range-s_part
    AND likp~fkstk IN (@zfi_000_cl02=>mc_fkstk_a, @zfi_000_cl02=>mc_fkstk_b)
    AND likp~gbstk IN (@zfi_000_cl02=>mc_gbstk_a,@zfi_000_cl02=>mc_gbstk_b)
    AND likp~vbtyp EQ @zfi_000_cl02=>mc_vbtyp_j.

*----------------------------------------*
*     MÜŞTERİ KOŞUL TABLOSU
*----------------------------------------*



    SELECT * FROM zfi_009_t01 INTO TABLE @DATA(lt_09) WHERE
      bukrs IN @ms_range-s_bukrs AND
      kunnr IN @ms_range-s_part .

    IF lt_kna1 IS NOT INITIAL.
      SELECT * FROM zfi_206_t01 INTO TABLE @DATA(lt_206)
        FOR ALL ENTRIES IN @lt_kna1
        WHERE kunnr EQ @lt_kna1-kunnr AND
              sevk_bit_tar GE @ms_range-p_keydt AND
              sevk_bas_tar LE @ms_range-p_keydt.
    ENDIF.
    SORT lt_206 BY sevk_bit_tar DESCENDING.


*----------------------------------------*
********     PROCESS DATA   *************
*----------------------------------------*

    LOOP AT lt_bsid REFERENCE INTO DATA(lr_bsid).
*
      CLEAR: ls_alv.
*
      IF lr_bsid->shkzg EQ zfi_000_cl02=>mc_shkzg_h.
        MULTIPLY lr_bsid->wrbtr BY -1 .
        MULTIPLY lr_bsid->dmbtr BY -1 .
        MULTIPLY lr_bsid->dmbe2 BY -1 .
        MULTIPLY lr_bsid->dmbe3 BY -1 .
      ENDIF.
      " vade tarihi
      CALL FUNCTION 'NET_DUE_DATE_GET'
        EXPORTING
          i_zfbdt = lr_bsid->zfbdt
          i_zbd1t = lr_bsid->zbd1t
          i_zbd2t = lr_bsid->zbd2t
          i_zbd3t = lr_bsid->zbd3t
          i_shkzg = lr_bsid->shkzg
          i_rebzg = lr_bsid->rebzg
          i_koart = zfi_000_cl02=>mc_koart_d
        IMPORTING
          e_faedt = lr_bsid->zfbdt.

      ls_alv-bukrs = lr_bsid->bukrs.
      ls_alv-kunnr = lr_bsid->kunnr.

      IF ms_range-p_konzs IS NOT INITIAL.

        READ TABLE lt_kna1 REFERENCE INTO DATA(lr_kna1)
        WITH KEY kunnr = lr_bsid->kunnr.

        IF sy-subrc IS INITIAL.
          IF  lr_kna1->konzs IS NOT INITIAL.
            ls_alv-konzs = lr_kna1->konzs .
          ENDIF.
        ENDIF.

      ENDIF.

*----------------------------------------------------*
*                     CH_BAKIYE
*----------------------------------------------------*

      IF lr_bsid->umskz EQ space.
        ADD lr_bsid->wrbtr TO ls_alv-ch_bakiye.
      ENDIF.


*----------------------------------------------------*
*                     ÇEKLER
*----------------------------------------------------*
      READ TABLE lt_bsed REFERENCE INTO DATA(lr_bsed)
      WITH KEY bukrs = lr_bsid->bukrs
               belnr = lr_bsid->belnr
               gjahr = lr_bsid->gjahr
               buzei = lr_bsid->buzei.

      IF sy-subrc IS INITIAL AND
        ( lr_bsid->umskz EQ zfi_000_cl02=>mc_umskz_1 OR
        lr_bsid->umskz EQ zfi_000_cl02=>mc_umskz_s ) .
        ADD lr_bsid->wrbtr TO ls_alv-c_toplam.

        IF lr_bsed->wstat = zfi_000_cl02=>mc_wstat_m.
          ADD lr_bsid->wrbtr TO ls_alv-vgkct.
          ADD lr_bsid->wrbtr TO ls_alv-vgckt.
        ELSE.
          ADD lr_bsid->wrbtr TO ls_alv-vgcckt.
          ADD lr_bsid->wrbtr TO ls_alv-vgckt.

          IF lr_bsid->zfbdt GE ms_range-p_keydt.
            ADD lr_bsid->wrbtr TO ls_alv-vgcckt2.
          ENDIF.

        ENDIF.


      ENDIF.

*----------------------------------------------------*
*                     EXIM E YÜKLÜ FATURA
*----------------------------------------------------*

      IF lr_bsid->xref3 EQ zfi_000_cl02=>mc_xref3_exim.

        ADD lr_bsid->wrbtr TO ls_alv-eli_teminat.

      ENDIF.

*----------------------------------------------------*
*                     TEMİNATLAR
*----------------------------------------------------*

      IF lr_bsid->umskz EQ zfi_000_cl02=>mc_umskz_tem_mek AND
         lr_bsid->zfbdt GE ms_range-p_keydt..
        SUBTRACT lr_bsid->wrbtr FROM ls_alv-tem_mek.
      ENDIF.

      IF lr_bsid->umskz EQ zfi_000_cl02=>mc_umskz_ipotek AND
         lr_bsid->zfbdt GE ms_range-p_keydt..
        SUBTRACT lr_bsid->wrbtr FROM ls_alv-ipotek.
      ENDIF.

      IF lr_bsid->umskz EQ zfi_000_cl02=>mc_umskz_exim AND
         lr_bsid->zfbdt GE ms_range-p_keydt..
        SUBTRACT lr_bsid->wrbtr FROM ls_alv-exim.
      ENDIF.

      IF lr_bsid->umskz EQ zfi_000_cl02=>mc_umskz_akreditif AND
         lr_bsid->zfbdt GE ms_range-p_keydt.
        SUBTRACT lr_bsid->wrbtr FROM ls_alv-akreditif.
      ENDIF.

*----------------------------------------------------*
*                     UPB & PB
*----------------------------------------------------*

      ls_alv-waers = lr_bsid->waers.

      COLLECT ls_alv INTO mt_alv.

    ENDLOOP.


*----------------------------------------------------*
*          zfi_009 tablosunda tanımlı cari
*----------------------------------------------------*

    LOOP AT lt_09 INTO DATA(lsx_09).

      CLEAR ls_alv.
      READ TABLE mt_alv INTO ls_alv WITH KEY bukrs = lsx_09-bukrs
                                             kunnr = lsx_09-kunnr.

      IF sy-subrc IS NOT INITIAL.
        CLEAR ls_alv.
        "MOVE-CORRESPONDING lsx_09 to
        ls_alv-bukrs = lsx_09-bukrs.
        ls_alv-kunnr = lsx_09-kunnr.
        APPEND ls_alv TO mt_alv .

      ENDIF.

    ENDLOOP.


*----------------------------------------------------*
*                   TASİLAT SÜRESİ
*----------------------------------------------------*
    s_augdt-sign   = 'I'.
    s_augdt-option = 'BT' .
    s_augdt-low    = ms_range-p_keydt .
    s_augdt-low(4)    = ms_range-p_keydt(4) - 1  .
    s_augdt-high   = ms_range-p_keydt .

    APPEND s_augdt TO r_augdt .

    DATA(it_bukrs) = VALUE tt_bukrs( FOR GROUPS grp OF wa IN mt_alv
                   GROUP BY ( bukrs = wa-bukrs )
                            "  waers = wa-waers )
                           ( bukrs = grp-bukrs ) ) .


    LOOP AT it_bukrs INTO DATA(ls_t001) .
      CREATE OBJECT lr_cl62.
      lr_cl62->ms_range-p_bukrs = ls_t001-bukrs .
      lr_cl62->ms_range-s_part = me->ms_range-s_part .
      lr_cl62->ms_range-s_augdt = r_augdt .
      lr_cl62->ms_range-p_erken = abap_true .
      lr_cl62->ms_range-p_tam = abap_true .
      lr_cl62->ms_range-p_gec = abap_true .
      lr_cl62->ms_range-p_iade = abap_true .
      lr_cl62->ms_range-p_odeme = abap_true .
      lr_cl62->mc_submit = abap_true .
      CALL METHOD lr_cl62->get_datas .

      DATA(lt_62_tmp) = lr_cl62->mt_alv .

      LOOP AT lt_62_tmp INTO DATA(ls_62_tmp) .
        APPEND ls_62_tmp TO lt_62_all .
      ENDLOOP .
    ENDLOOP .



    LOOP AT mt_alv REFERENCE INTO DATA(lr_alv).

*----------------------------------------------------*
*                     DBS LİMİT ve KDT
*----------------------------------------------------*

      LOOP AT lt_dbs REFERENCE INTO DATA(lr_dbs)
        WHERE kunnr EQ lr_alv->kunnr AND
              bukrs EQ lr_alv->bukrs.
        ADD lr_dbs->dbslm TO lr_alv->dbslm.
        ADD lr_dbs->riskl TO lr_alv->dbs_kredi.

      ENDLOOP.

*----------------------------------------------------*
*                     müşteri ana veri
*----------------------------------------------------*
      READ TABLE lt_kna1 REFERENCE INTO lr_kna1
      WITH KEY kunnr = lr_alv->kunnr
               bukrs = lr_alv->bukrs .

      IF sy-subrc IS INITIAL.
        lr_alv->name1 = lr_kna1->name1 .
        lr_alv->bland = lr_kna1->regio .
        lr_alv->vtweg = lr_kna1->vtweg .
        lr_alv->vkorg = lr_kna1->vkorg .
        lr_alv->bezei = lr_kna1->bezei .
        lr_alv->vtext = lr_kna1->vtext .
        lr_alv->stcd2 = lr_kna1->stcd2 .

        IF lr_kna1->zterm IS NOT INITIAL.
          READ TABLE lt_052 REFERENCE INTO DATA(lr_052)
          WITH KEY zterm = lr_kna1->zterm.
          IF sy-subrc IS INITIAL.

            lr_alv->s_vade = lr_052->ztag1 .

          ENDIF.

          lr_alv->g_vade = + lr_alv->s_vade
                           + lr_kna1->kultg .

        ENDIF.

      ENDIF.

      READ TABLE lt_206 INTO DATA(ls_206)
      WITH KEY kunnr = lr_alv->kunnr
         vkorg = lr_alv->vkorg
         vtweg = lr_alv->vtweg .

      IF sy-subrc IS INITIAL.
        ADD ls_206-ek_vade TO lr_alv->g_vade .
      ENDIF.

*----------------------------------------------------*
*              TESLİMAT TUTARI
*----------------------------------------------------*

      LOOP AT lt_lips REFERENCE INTO DATA(lr_lips)
        WHERE bukrs EQ lr_alv->bukrs AND
              kunag EQ lr_alv->kunnr AND
              vkorg EQ lr_alv->vkorg AND
              vtweg EQ lr_alv->vtweg AND
              waerk EQ lr_alv->waers.

        CHECK lr_lips->kwmeng GT 0.

        CLEAR: tutar_bpb, tutar_upb .
        tutar_bpb = ( lr_lips->netwr  + lr_lips->mwsbp   )
        / lr_lips->kwmeng * lr_lips->lfimg.


*        CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
*          EXPORTING
**           CLIENT           = SY-MANDT
*            date             = ms_range-p_keydt
*            foreign_amount   = tutar_bpb
*            foreign_currency = lr_lips->waerk
*            local_currency   = lr_alv->waers
**           RATE             = 0
*            type_of_rate     = mc_type_of_rate_m
*            read_tcurr       = abap_true
*          IMPORTING
**           EXCHANGE_RATE    =
**           FOREIGN_FACTOR   =
*            local_amount     = tutar_upb
**           LOCAL_FACTOR     =
**           EXCHANGE_RATEX   =
**           FIXED_RATE       =
**           DERIVED_RATE_TYPE       =
*          EXCEPTIONS
*            no_rate_found    = 1
*            overflow         = 2
*            no_factors_found = 3
*            no_spread_found  = 4
*            derived_2_times  = 5
*            OTHERS           = 6.
*        IF sy-subrc <> 0.
**         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*        ENDIF.


        IF lr_lips->wbstk EQ zfi_000_cl02=>mc_wbstk_c.
*          ADD tutar_upb TO lr_alv->t_tutar.
        ELSE.
          ADD tutar_bpb TO lr_alv->t_tutar .

        ENDIF.

      ENDLOOP.

**----------------------------------------------------*
**                FORMÜLLER
**----------------------------------------------------*


* toplam risk
      lr_alv->t_risk =  + lr_alv->ch_bakiye
                        + lr_alv->t_tutar
                        + lr_alv->c_toplam .



* toplam teminat exim ve cek dahil
      lr_alv->e_c_t_teminat = + lr_alv->tem_mek
                              + lr_alv->dbslm
                              + lr_alv->ipotek
                              + lr_alv->exim
                              + lr_alv->cek_tem_tutar.

* toplam teminat
      lr_alv->t_teminat =     + lr_alv->tem_mek
                              + lr_alv->dbslm
                              - lr_alv->dbs_kredi
                              + lr_alv->ipotek
                              + lr_alv->exim
                              + lr_alv->akreditif
.

* toplam teminatlı risk
      IF lr_alv->e_c_t_teminat LE lr_alv->t_risk.

        lr_alv->t_tli_risk = lr_alv->e_c_t_teminat.

      ELSE.

        lr_alv->t_tli_risk = lr_alv->t_risk .

      ENDIF.

      IF lr_alv->t_tli_risk LT 0 .
        lr_alv->t_tli_risk = 0 .
      ENDIF.

* toplam teminatsız risk
      lr_alv->tsiz_risk =  + lr_alv->t_risk
                           - lr_alv->t_tli_risk .

      IF lr_alv->tsiz_risk LE 0 .

        lr_alv->tsiz_risk = 0 .

      ENDIF.

* toplam müşteri kredi limiti

      READ TABLE lt_09 INTO DATA(ls_09)
      WITH KEY kunnr = lr_alv->kunnr .
      " peşin ve limitsiz tıkları

      IF sy-subrc IS INITIAL.
        lr_alv->limitsiz_x = ls_09-limitsiz_x.
        lr_alv->pesin_x = ls_09-pesin_x.
      ENDIF.

      DATA(lv_teminat) = + lr_alv->t_teminat
                         - lr_alv->exim.

      lr_alv->toplam = lv_teminat.

      " koşul teminatı
      IF ls_09-teminat_x EQ abap_true .
        IF ( ( lv_teminat ) * ls_09-teminat / 100 ) LE ls_09-max_teminat .
          lr_alv->toplam = + lr_alv->toplam
                           + ( lv_teminat * ls_09-teminat / 100 ) .
          lr_alv->teminat_x = ( lv_teminat * ls_09-teminat / 100 ) .
        ELSE.
          lr_alv->toplam = + lr_alv->toplam
                           + ls_09-max_teminat .
          lr_alv->teminat_x = ls_09-max_teminat .
        ENDIF.

      ENDIF.

      " sabit açık limiti
      IF ls_09-tarih GE ms_range-p_keydt AND
         ls_09-tutar_x EQ abap_true.
        lr_alv->toplam = + lr_alv->toplam
                         + ls_09-tutar .
        lr_alv->sabit_x = ls_09-tutar .
      ENDIF.

      " cirolu çek teminatı
      IF ls_09-cirolu_x EQ abap_true .

        IF ( lr_alv->vgcckt2 * ls_09-cirolu / 100 ) LE ls_09-cirolu_max .
          lr_alv->toplam = + lr_alv->toplam
                           + ( lr_alv->vgcckt2 * ls_09-cirolu / 100 ) .
          lr_alv->cek_tem_tutar = ( lr_alv->vgcckt2 * ls_09-cirolu / 100 ).
        ELSE.
          lr_alv->toplam = + lr_alv->toplam
                           + ls_09-cirolu_max .
          lr_alv->cek_tem_tutar = ls_09-cirolu_max .
        ENDIF.

      ENDIF.

* exim ve çek dahil toplam teminat
      lr_alv->e_c_t_teminat = + lr_alv->tem_mek
                              + lr_alv->dbslm
                              + lr_alv->ipotek
                              + lr_alv->exim
                              + lr_alv->cek_tem_tutar.

      " exim limiti

      IF ls_09-exim_x EQ abap_true .
        ADD lr_alv->exim TO lr_alv->toplam .
      ENDIF.



      " son 1 sene tahsilat süresi

      READ TABLE lt_62_all INTO DATA(ls_62)
      WITH KEY partner = lr_alv->kunnr
               bukrs   = lr_alv->bukrs.

      IF sy-subrc IS INITIAL.

        lr_alv->f_t_vade = ls_62-ogun .
        lr_alv->f_f_vade = ls_62-fgun .

      ENDIF.

      " limit blokajı
      IF lr_alv->toplam LT lr_alv->t_risk.
        lr_alv->limit_b = abap_true.
      ENDIF.

      " Kullanılabilir Limit
      lr_alv->kul_limit = + lr_alv->toplam
                          - lr_alv->t_risk .

    ENDLOOP.

    SORT mt_alv BY  bukrs kunnr .


  ENDMETHOD.


  METHOD up.


*----------------------------------------*
*     DATA DEFINATIONS
*----------------------------------------*

    DATA: ls_alv LIKE LINE OF mt_alv.
    DATA: lv_tabix TYPE sy-tabix.
    DATA: var_soll TYPE knc1-umsav.
    DATA: var_haben TYPE knc1-umsav.
    DATA: wa_knc1 TYPE knc1 .
    DATA: wa_knb4 TYPE knb4 .
    DATA: tarih TYPE kaljahr.
    DATA: jahxx TYPE kaljahr.
    DATA: var_ag  TYPE knb4-agn01.
    DATA: var_vz  TYPE knb4-vzn01.
    DATA: ct      TYPE i.
    DATA: prod(8) TYPE p.
    DATA: toplam           TYPE fdbl_de_bal,  "agsxx,
          tutar_bpb        TYPE fdbl_de_bal,  "agsxx,
          tutar_upb        TYPE fdbl_de_bal,  "agsxx,
          toplam_tum       TYPE fdbl_de_bal,
          vade_asimi       TYPE p DECIMALS 0,  "vzsxx,
          vade_asimi_tum   TYPE p DECIMALS 0, "vzsxx,
          ag_toplam        TYPE fdbl_de_bal,
          alacak_toplam    TYPE fdbl_de_bal,
          ag_alacak_toplam TYPE fdbl_de_bal,
          borc_toplam      TYPE fdbl_de_bal,
          ag_borc_toplam   TYPE fdbl_de_bal,
          ag_toplam_tum    TYPE fdbl_de_bal.
    DATA: monxx TYPE knb4-mon01,    " Monat
          agsxx TYPE knb4-ags01,    " Ausgleich Skonto 1
          vzsxx TYPE knb4-vzs01,    " Verzugstage Skonto 1
          agnxx TYPE knb4-agn01,    " Ausgleich Netto
          vznxx TYPE knb4-vzn01,    " Verzugstage Netto
          anzxx TYPE knb4-anz01.
    DATA : r_kunnr TYPE RANGE OF kna1-kunnr .
    DATA : r_stcd2 TYPE RANGE OF kna1-stcd2 .

    DATA: lt_fatura TYPE TABLE OF bsid.

    DATA : lr_cl62 TYPE REF TO zfi_062_cl02 .
    DATA : ls_62_s05 TYPE zfi_062_s05 .
    DATA : lt_62_all TYPE zfi_062_s06_tt01 .
    DATA : r_augdt TYPE zbc_000_tt02 .
    DATA : s_augdt TYPE LINE OF zbc_000_tt02.
    TYPES : BEGIN OF ty_bukrs,
              bukrs TYPE bukrs,
            END OF ty_bukrs,
            tt_bukrs TYPE STANDARD TABLE OF ty_bukrs WITH DEFAULT KEY.

*----------------------------------------*
*     ANAVERİ
*----------------------------------------*

    SELECT a~kunnr, a~land1, a~name1, a~stcd2,
           a~regio, b~bukrs, a~konzs, c~vkorg,
           c~vtweg, c~vkbur, c~vkgrp, c~kdgrp,
           c~zterm, e~bezei, f~vtext, b~kultg
    FROM kna1 AS a
         INNER JOIN knb1 AS b ON b~kunnr EQ a~kunnr
         LEFT JOIN knvv AS c  ON c~kunnr EQ b~kunnr
         LEFT JOIN tvko AS d  ON d~vkorg EQ c~vkorg
                             AND d~bukrs EQ b~bukrs
         LEFT JOIN t005u AS e ON a~land1 EQ e~land1
                             AND a~regio EQ e~bland
                             AND e~spras EQ @sy-langu
         LEFT JOIN tvtwt AS f ON c~vtweg EQ f~vtweg
                             AND f~spras EQ @sy-langu
    WHERE a~kunnr IN @ms_range-s_part AND
          b~bukrs IN @ms_range-s_bukrs AND
          b~bukrs NE @space AND
          c~vkorg IN @ms_range-s_vkorg AND
          c~vkgrp IN @ms_range-s_vkgrp AND
          c~vkbur IN @ms_range-s_vkbur AND
          c~kdgrp IN @ms_range-s_kdgrp AND
          c~vtweg IN @ms_range-s_vtweg AND
           ( ( c~vkorg IS NULL AND d~vkorg IS NULL ) OR
             ( b~bukrs EQ d~bukrs ) )
             ORDER BY f~vtweg
    INTO TABLE @DATA(lt_kna1) .


    "" her durumda aynı vkn'li müşterileri getirsin
*    IF ms_range-p_vergi IS NOT INITIAL OR
*       mv_vergi IS NOT INITIAL.
    " aynı vergi no da olan cariler çekilecek
    " r_part dolacak , alttaki selectte de rpart kullanılacak
    IF lt_kna1[] IS NOT INITIAL.
      SELECT a~kunnr, a~land1, a~name1, a~stcd2,
             a~regio, b~bukrs, a~konzs, c~vkorg,
             c~vtweg, c~vkbur, c~vkgrp, c~kdgrp,
             c~zterm, e~bezei, f~vtext, b~kultg
      FROM kna1 AS a
           INNER JOIN knb1 AS b ON b~kunnr EQ a~kunnr
           LEFT JOIN knvv AS c  ON c~kunnr EQ b~kunnr
           LEFT JOIN tvko AS d  ON d~vkorg EQ c~vkorg
                               AND d~bukrs EQ b~bukrs
           LEFT JOIN t005u AS e ON a~land1 EQ e~land1
                               AND a~regio EQ e~bland
                               AND e~spras EQ @sy-langu
           LEFT JOIN tvtwt AS f ON c~vtweg EQ f~vtweg
                               AND f~spras EQ @sy-langu
        FOR ALL ENTRIES IN @lt_kna1
      WHERE a~stcd2 EQ @lt_kna1-stcd2 AND
            a~kunnr NE @lt_kna1-kunnr AND
            a~stcd2 NE '' AND
            b~bukrs IN @ms_range-s_bukrs AND
            b~bukrs NE @space AND
            c~vkorg IN @ms_range-s_vkorg AND
            c~vkgrp IN @ms_range-s_vkgrp AND
            c~vkbur IN @ms_range-s_vkbur AND
            c~kdgrp IN @ms_range-s_kdgrp AND
            c~vtweg IN @ms_range-s_vtweg AND
             ( ( c~vkorg IS NULL AND d~vkorg IS NULL ) OR
               ( b~bukrs EQ d~bukrs ) )
*             ORDER BY f~vtweg
      APPENDING TABLE @lt_kna1 .

    ENDIF.
    SORT lt_kna1 BY kunnr vtweg .
    DELETE ADJACENT DUPLICATES FROM lt_kna1 .
    SORT lt_kna1 BY vtweg kunnr .
    DELETE ADJACENT DUPLICATES FROM lt_kna1 COMPARING vtweg kunnr .
*    ENDIF.


    SELECT * FROM t052
      INTO TABLE @DATA(lt_052) .




*----------------------------------------*
*     AÇIK KALEMLER
*----------------------------------------*
    IF lt_kna1 IS NOT INITIAL.

      SELECT * FROM bsid INTO TABLE @DATA(lt_bsid)
        FOR ALL ENTRIES IN @lt_kna1
      WHERE bukrs EQ @lt_kna1-bukrs AND
            kunnr EQ @lt_kna1-kunnr AND
            budat LE @ms_range-p_keydt .

      SELECT * FROM bsad APPENDING TABLE lt_bsid
               FOR ALL ENTRIES IN lt_kna1
      WHERE bukrs EQ lt_kna1-bukrs AND
            kunnr EQ lt_kna1-kunnr AND
               budat LE ms_range-p_keydt AND
               augdt GT ms_range-p_keydt .
    ENDIF.
*----------------------------------------*
*     ÇEK DURUMU
*----------------------------------------*
    IF lt_bsid IS NOT INITIAL.

      SELECT * FROM bsed INTO TABLE @DATA(lt_bsed)
        FOR ALL ENTRIES IN @lt_bsid
        WHERE bukrs EQ @lt_bsid-bukrs AND
              belnr EQ @lt_bsid-belnr AND
              gjahr EQ @lt_bsid-gjahr AND
              buzei EQ @lt_bsid-buzei .

    ENDIF.
*----------------------------------------*
*     DBS alanları
*----------------------------------------*
    IF lt_kna1 IS NOT INITIAL.
      SELECT * FROM zfi_089_t03 INTO TABLE @DATA(lt_dbs)
        FOR ALL ENTRIES IN @lt_kna1
              WHERE bukrs EQ @lt_kna1-bukrs AND
                  kunnr   EQ @lt_kna1-kunnr .

    ENDIF.


*----------------------------------------*
*     ORGANİZASYONEL DATA
*----------------------------------------*
    " lt_but000 tablosu hiç bir yerde kullanılmadığı için kapatılmıştır.
* selectte alan belirtilmediği için ATC Checkde takılıyor.
*    SELECT * FROM but000 INTO TABLE @DATA(lt_but000)
*      WHERE partner IN @ms_range-s_part.

    SELECT * FROM t001  INTO TABLE @DATA(lt_001)
      WHERE bukrs IN @ms_range-s_bukrs.

*    IF lt_bsid IS INITIAL."kullanılmadığı için atc check hatası veriyor kapatıldı xd_fatihk
*      SELECT * FROM bkpf INTO TABLE @DATA(lt_bkpf)
*        FOR ALL ENTRIES IN @lt_bsid
*        WHERE bukrs EQ @lt_bsid-bukrs AND
*              belnr EQ @lt_bsid-belnr AND
*              gjahr EQ @lt_bsid-gjahr.
*    ENDIF.


*----------------------------------------*
*     SİPARİŞ VE TESLİMAT
*----------------------------------------*

    SELECT
       tvko~bukrs ,
       vbak~vkorg,
       vbak~kunnr,
       vbap~vbeln,
       vbap~posnr,
       vbak~lifsk,
       vbak~waerk,
       vbap~kwmeng,
       vbap~netwr,
       vbap~mwsbp,
       vbap~lfsta
    INTO TABLE @DATA(lt_vbap)
    FROM vbak AS vbak INNER JOIN vbap AS vbap
    ON vbak~vbeln EQ vbap~vbeln
    INNER JOIN tvko AS tvko
    ON vbak~vkorg EQ tvko~vkorg
    WHERE vbak~kunnr IN @ms_range-s_part
    AND tvko~bukrs IN @ms_range-s_bukrs
    AND vbak~vbtyp EQ @zfi_000_cl02=>mc_vbtyp_c
    AND vbap~absta NE @zfi_000_cl02=>mc_absta_c
    AND vbap~gbsta NE @zfi_000_cl02=>mc_gbsta_c
    AND vbap~lfsta IN (@zfi_000_cl02=>mc_lfsta_a,@zfi_000_cl02=>mc_lfsta_b).


    SELECT lips~vbeln,
           lips~posnr,
           lips~lfimg,
           likp~wbstk,
           likp~kunag,
           vbap~netwr,
           vbap~mwsbp,
           vbap~kwmeng,
           vbap~waerk,
           tvko~bukrs,
           likp~vkorg,
           lips~vtweg
    INTO  TABLE @DATA(lt_lips)
    FROM likp AS likp
    INNER JOIN lips AS lips
    ON likp~vbeln EQ lips~vbeln
    INNER JOIN vbap AS vbap
    ON lips~vgbel EQ vbap~vbeln AND
    lips~vgpos EQ vbap~posnr
    INNER JOIN tvko AS tvko
    ON likp~vkorg EQ tvko~vkorg
    WHERE tvko~bukrs IN @ms_range-s_bukrs
    AND likp~kunag IN @ms_range-s_part
    AND likp~fkstk IN (@zfi_000_cl02=>mc_fkstk_a, @zfi_000_cl02=>mc_fkstk_b)
    AND likp~gbstk IN (@zfi_000_cl02=>mc_gbstk_a,@zfi_000_cl02=>mc_gbstk_b)
    AND likp~vbtyp EQ @zfi_000_cl02=>mc_vbtyp_j.

*----------------------------------------*
*     MÜŞTERİ KOŞUL TABLSO
*----------------------------------------*



    SELECT * FROM zfi_009_t01 INTO TABLE @DATA(lt_09) WHERE
      bukrs IN @ms_range-s_bukrs AND
      kunnr IN @ms_range-s_part .

    IF lt_kna1 IS NOT INITIAL.

      SELECT * FROM zfi_206_t01 INTO TABLE @DATA(lt_206)
        FOR ALL ENTRIES IN @lt_kna1
        WHERE kunnr EQ @lt_kna1-kunnr AND
              sevk_bit_tar GE @ms_range-p_keydt AND
              sevk_bas_tar LE @ms_range-p_keydt.

    ENDIF.
    SORT lt_206 BY sevk_bit_tar DESCENDING.

*----------------------------------------*
********     PROCESS DATA   *************
*----------------------------------------*

    LOOP AT lt_bsid REFERENCE INTO DATA(lr_bsid).
*
      CLEAR: ls_alv.
*
      IF lr_bsid->shkzg EQ zfi_000_cl02=>mc_shkzg_h.
        MULTIPLY lr_bsid->dmbtr BY -1 .
        MULTIPLY lr_bsid->wrbtr BY -1 .
        MULTIPLY lr_bsid->dmbe2 BY -1 .
        MULTIPLY lr_bsid->dmbe3 BY -1 .
      ENDIF.
      " vade tarihi
      CALL FUNCTION 'NET_DUE_DATE_GET'
        EXPORTING
          i_zfbdt = lr_bsid->zfbdt
          i_zbd1t = lr_bsid->zbd1t
          i_zbd2t = lr_bsid->zbd2t
          i_zbd3t = lr_bsid->zbd3t
          i_shkzg = lr_bsid->shkzg
          i_rebzg = lr_bsid->rebzg
          i_koart = zfi_000_cl02=>mc_koart_d
        IMPORTING
          e_faedt = lr_bsid->zfbdt.

      ls_alv-bukrs = lr_bsid->bukrs.
      ls_alv-kunnr = lr_bsid->kunnr.

      IF ms_range-p_konzs IS NOT INITIAL.

        READ TABLE lt_kna1 REFERENCE INTO DATA(lr_kna1)
        WITH KEY kunnr = lr_bsid->kunnr.

        IF sy-subrc IS INITIAL.
          IF  lr_kna1->konzs IS NOT INITIAL.
            ls_alv-konzs = lr_kna1->konzs .
          ENDIF.
        ENDIF.

      ENDIF.

*----------------------------------------------------*
*                     CH_BAKIYE
*----------------------------------------------------*

      IF lr_bsid->umskz EQ space.
        ADD lr_bsid->dmbtr TO ls_alv-ch_bakiye.
      ENDIF.


*----------------------------------------------------*
*                     ÇEKLER
*----------------------------------------------------*
      READ TABLE lt_bsed REFERENCE INTO DATA(lr_bsed)
      WITH KEY bukrs = lr_bsid->bukrs
               belnr = lr_bsid->belnr
               gjahr = lr_bsid->gjahr
               buzei = lr_bsid->buzei.

      IF sy-subrc IS INITIAL AND
        ( lr_bsid->umskz EQ zfi_000_cl02=>mc_umskz_1 OR
        lr_bsid->umskz EQ zfi_000_cl02=>mc_umskz_s ) .
        ADD lr_bsid->dmbtr TO ls_alv-c_toplam.

        IF lr_bsed->wstat = zfi_000_cl02=>mc_wstat_m.
          ADD lr_bsid->dmbtr TO ls_alv-vgkct.
          ADD lr_bsid->dmbtr TO ls_alv-vgckt.
        ELSE.
          ADD lr_bsid->dmbtr TO ls_alv-vgcckt.
          ADD lr_bsid->dmbtr TO ls_alv-vgckt.

          IF lr_bsid->zfbdt GE ms_range-p_keydt.
            ADD lr_bsid->dmbtr TO ls_alv-vgcckt2.
          ENDIF.

        ENDIF.


      ENDIF.
*----------------------------------------------------*
*                     EXIM E YÜKLÜ FATURA
*----------------------------------------------------*

      IF lr_bsid->xref3 EQ zfi_000_cl02=>mc_xref3_exim.

        ADD lr_bsid->dmbtr TO ls_alv-eli_teminat.

      ENDIF.


*----------------------------------------------------*
*                     TEMİNATLAR
*----------------------------------------------------*

      IF lr_bsid->umskz EQ zfi_000_cl02=>mc_umskz_tem_mek  AND
         lr_bsid->zfbdt GE ms_range-p_keydt.
        SUBTRACT lr_bsid->dmbtr FROM ls_alv-tem_mek .
      ENDIF.

      IF lr_bsid->umskz EQ zfi_000_cl02=>mc_umskz_ipotek AND
         lr_bsid->zfbdt GE ms_range-p_keydt.
        SUBTRACT lr_bsid->dmbtr FROM ls_alv-ipotek.
      ENDIF.

      IF lr_bsid->umskz EQ zfi_000_cl02=>mc_umskz_exim AND
         lr_bsid->zfbdt GE ms_range-p_keydt.
        SUBTRACT lr_bsid->dmbtr FROM ls_alv-exim.
      ENDIF.

      IF lr_bsid->umskz EQ zfi_000_cl02=>mc_umskz_akreditif AND
         lr_bsid->zfbdt GE ms_range-p_keydt.
        SUBTRACT lr_bsid->dmbtr FROM ls_alv-akreditif.
      ENDIF.

*----------------------------------------------------*
*                     UPB & PB
*----------------------------------------------------*

      READ TABLE lt_001 REFERENCE INTO DATA(lr_001)
      WITH KEY bukrs = lr_bsid->bukrs.
      IF sy-subrc IS INITIAL.
        ls_alv-waers = lr_001->waers.
      ENDIF.


      COLLECT ls_alv INTO mt_alv.

    ENDLOOP.

*----------------------------------------------------*
*          zfi_009 tablosunda tanımlı cari
*----------------------------------------------------*

    LOOP AT lt_09 INTO DATA(lsx_09).

      CLEAR ls_alv.
      READ TABLE mt_alv INTO ls_alv WITH KEY bukrs = lsx_09-bukrs
                                             kunnr = lsx_09-kunnr.

      IF sy-subrc IS NOT INITIAL.
        CLEAR ls_alv.
        "MOVE-CORRESPONDING lsx_09 to
        ls_alv-bukrs = lsx_09-bukrs.
        ls_alv-kunnr = lsx_09-kunnr.

        READ TABLE lt_001 REFERENCE INTO lr_001
        WITH KEY bukrs = lsx_09-bukrs.
        IF sy-subrc IS INITIAL.
          ls_alv-waers = lr_001->waers .
        ENDIF.

        APPEND ls_alv TO mt_alv .

      ENDIF.

    ENDLOOP.


*----------------------------------------------------*
*                   TASİLAT SÜRESİ
*----------------------------------------------------*
    s_augdt-sign   = 'I'.
    s_augdt-option = 'BT' .
    s_augdt-low    = ms_range-p_keydt .
    s_augdt-low(4)    = ms_range-p_keydt(4) - 1  .
    s_augdt-high   = ms_range-p_keydt .

    APPEND s_augdt TO r_augdt .

    DATA(it_bukrs) = VALUE tt_bukrs( FOR GROUPS grp OF wa IN mt_alv
                   GROUP BY ( bukrs = wa-bukrs )
                            "  waers = wa-waers )
                           ( bukrs = grp-bukrs ) ) .


    LOOP AT it_bukrs INTO DATA(ls_t001) .
      CREATE OBJECT lr_cl62.
      lr_cl62->ms_range-p_bukrs = ls_t001-bukrs .
      lr_cl62->ms_range-s_part = me->ms_range-s_part .
      lr_cl62->ms_range-s_augdt = r_augdt .
      lr_cl62->ms_range-p_erken = abap_true .
      lr_cl62->ms_range-p_tam = abap_true .
      lr_cl62->ms_range-p_gec = abap_true .
      lr_cl62->ms_range-p_iade = abap_true .
      lr_cl62->ms_range-p_odeme = abap_true .
      lr_cl62->mc_submit = abap_true .
      CALL METHOD lr_cl62->get_datas .

      DATA(lt_62_tmp) = lr_cl62->mt_alv .

      LOOP AT lt_62_tmp INTO DATA(ls_62_tmp) .
        APPEND ls_62_tmp TO lt_62_all .
      ENDLOOP .
    ENDLOOP .



    LOOP AT mt_alv REFERENCE INTO DATA(lr_alv).

*----------------------------------------------------*
*                     DBS LİMİT ve KDT
*----------------------------------------------------*

      LOOP AT lt_dbs REFERENCE INTO DATA(lr_dbs)
        WHERE kunnr EQ lr_alv->kunnr AND
              bukrs EQ lr_alv->bukrs.
        ADD lr_dbs->dbslm TO lr_alv->dbslm.
        ADD lr_dbs->riskl TO lr_alv->dbs_kredi.

      ENDLOOP.

*----------------------------------------------------*
*                     müşteri ana veri ve vade
*----------------------------------------------------*
      READ TABLE lt_kna1 REFERENCE INTO lr_kna1
      WITH KEY kunnr = lr_alv->kunnr
               bukrs = lr_alv->bukrs .

      IF sy-subrc IS INITIAL.
        lr_alv->name1 = lr_kna1->name1 .
        lr_alv->bland = lr_kna1->regio .
        lr_alv->vtweg = lr_kna1->vtweg .
        lr_alv->vkorg = lr_kna1->vkorg .
        lr_alv->bezei = lr_kna1->bezei .
        lr_alv->vtext = lr_kna1->vtext .
        lr_alv->stcd2 = lr_kna1->stcd2 .

        IF lr_kna1->zterm IS NOT INITIAL.
          READ TABLE lt_052 REFERENCE INTO DATA(lr_052)
          WITH KEY zterm = lr_kna1->zterm.
          IF sy-subrc IS INITIAL.

            lr_alv->s_vade = lr_052->ztag1 .

          ENDIF.

          lr_alv->g_vade = + lr_alv->s_vade
                           + lr_kna1->kultg .


        ENDIF.

      ENDIF.

      READ TABLE lt_206 INTO DATA(ls_206)
      WITH KEY kunnr = lr_alv->kunnr
               vkorg = lr_alv->vkorg
               vtweg = lr_alv->vtweg .

      IF sy-subrc IS INITIAL.
        ADD ls_206-ek_vade TO lr_alv->g_vade .
      ENDIF.

*----------------------------------------------------*
*              TESLİMAT TUTARI
*----------------------------------------------------*

      LOOP AT lt_lips REFERENCE INTO DATA(lr_lips)
        WHERE bukrs EQ lr_alv->bukrs AND
              kunag EQ lr_alv->kunnr AND
              vkorg EQ lr_alv->vkorg AND
              vtweg EQ lr_alv->vtweg.

        CHECK lr_lips->kwmeng GT 0.

        CLEAR: tutar_bpb, tutar_upb .
        tutar_bpb = ( lr_lips->netwr  + lr_lips->mwsbp   )
        / lr_lips->kwmeng * lr_lips->lfimg.


        CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
          EXPORTING
*           CLIENT           = SY-MANDT
            date             = ms_range-p_keydt
            foreign_amount   = tutar_bpb
            foreign_currency = lr_lips->waerk
            local_currency   = lr_alv->waers
*           RATE             = 0
            type_of_rate     = zfi_000_cl02=>mc_type_of_rate_m
            read_tcurr       = abap_true
          IMPORTING
*           EXCHANGE_RATE    =
*           FOREIGN_FACTOR   =
            local_amount     = tutar_upb
*           LOCAL_FACTOR     =
*           EXCHANGE_RATEX   =
*           FIXED_RATE       =
*           DERIVED_RATE_TYPE       =
          EXCEPTIONS
            no_rate_found    = 1
            overflow         = 2
            no_factors_found = 3
            no_spread_found  = 4
            derived_2_times  = 5
            OTHERS           = 6.
        IF sy-subrc <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.


        IF lr_lips->wbstk EQ zfi_000_cl02=>mc_wbstk_c.
*          ADD tutar_upb TO lr_alv->t_tutar.
        ELSE.
          ADD tutar_upb TO lr_alv->t_tutar .

        ENDIF.

      ENDLOOP.

**----------------------------------------------------*
**                FORMÜLLER
**----------------------------------------------------*


* toplam risk
      lr_alv->t_risk =  + lr_alv->ch_bakiye
                        + lr_alv->t_tutar
                        + lr_alv->c_toplam .



* toplam teminat exim ve cek dahil
      lr_alv->e_c_t_teminat = + lr_alv->tem_mek
                              + lr_alv->dbslm
                              + lr_alv->ipotek
                              + lr_alv->exim
                              + lr_alv->cek_tem_tutar.

* toplam teminat
      lr_alv->t_teminat =     + lr_alv->tem_mek
                              + lr_alv->dbslm
                              - lr_alv->dbs_kredi
                              + lr_alv->ipotek
                              + lr_alv->exim
                              + lr_alv->akreditif
.

* toplam teminatlı risk
      IF lr_alv->e_c_t_teminat LE lr_alv->t_risk.

        lr_alv->t_tli_risk = lr_alv->e_c_t_teminat.

      ELSE.

        lr_alv->t_tli_risk = lr_alv->t_risk .

      ENDIF.

      IF lr_alv->t_tli_risk LT 0 .
        lr_alv->t_tli_risk = 0 .
      ENDIF.

* toplam teminatsız risk
      lr_alv->tsiz_risk =  + lr_alv->t_risk
                           - lr_alv->t_tli_risk .

      IF lr_alv->tsiz_risk LE 0 .

        lr_alv->tsiz_risk = 0 .

      ENDIF.

* toplam müşteri kredi limiti

      READ TABLE lt_09 INTO DATA(ls_09)
      WITH KEY kunnr = lr_alv->kunnr .
      " peşin ve limitsiz tıkları

      IF sy-subrc IS INITIAL.
        lr_alv->limitsiz_x = ls_09-limitsiz_x.
        lr_alv->pesin_x = ls_09-pesin_x.
      ENDIF.

      DATA(lv_teminat) = + lr_alv->t_teminat
                         - lr_alv->exim.

      lr_alv->toplam = lv_teminat.

      " koşul teminatı
      IF ls_09-teminat_x EQ abap_true .
        IF ( ( lv_teminat ) * ls_09-teminat / 100 ) LE ls_09-max_teminat .
          lr_alv->toplam = + lr_alv->toplam
                           + ( lv_teminat * ls_09-teminat / 100 ) .
          lr_alv->teminat_x = ( lv_teminat * ls_09-teminat / 100 ) .
        ELSE.
          lr_alv->toplam = + lr_alv->toplam
                           + ls_09-max_teminat .
          lr_alv->teminat_x = ls_09-max_teminat .
        ENDIF.

      ENDIF.

      " sabit açık limiti
      IF ls_09-tarih GE ms_range-p_keydt AND
         ls_09-tutar_x EQ abap_true.
        lr_alv->toplam = + lr_alv->toplam
                         + ls_09-tutar .
        lr_alv->sabit_x = ls_09-tutar .
      ENDIF.

      " cirolu çek teminatı
      IF ls_09-cirolu_x EQ abap_true .

        IF ( lr_alv->vgcckt2 * ls_09-cirolu / 100 ) LE ls_09-cirolu_max .
          lr_alv->toplam = + lr_alv->toplam
                           + ( lr_alv->vgcckt2 * ls_09-cirolu / 100 ) .
          lr_alv->cek_tem_tutar = ( lr_alv->vgcckt2 * ls_09-cirolu / 100 ).
        ELSE.
          lr_alv->toplam = + lr_alv->toplam
                           + ls_09-cirolu_max .
          lr_alv->cek_tem_tutar = ls_09-cirolu_max .
        ENDIF.

      ENDIF.

* exim ve çek dahil toplam teminat
      lr_alv->e_c_t_teminat = + lr_alv->tem_mek
                              + lr_alv->dbslm
                              + lr_alv->ipotek
                              + lr_alv->exim
                              + lr_alv->cek_tem_tutar.

      " exim limiti

      IF ls_09-exim_x EQ abap_true .
        ADD lr_alv->exim TO lr_alv->toplam .
      ENDIF.

      " son 1 sene tahsilat süresi

      READ TABLE lt_62_all INTO DATA(ls_62)
      WITH KEY partner = lr_alv->kunnr
               bukrs   = lr_alv->bukrs.

      IF sy-subrc IS INITIAL.

        lr_alv->f_t_vade = ls_62-ogun .
        lr_alv->f_f_vade = ls_62-fgun .

      ENDIF.

      " limit blokajı

      IF lr_alv->toplam LT lr_alv->t_risk.
        lr_alv->limit_b = abap_true.
      ENDIF.

      " Kullanılabilir Limit
      lr_alv->kul_limit = + lr_alv->toplam
                          - lr_alv->t_risk .


    ENDLOOP.

    SORT mt_alv BY  bukrs kunnr .


  ENDMETHOD.


  METHOD ZBC_000_IF01~USER_COMMAND.


*    CASE iv_ucomm                 .
*      WHEN 'SIMULATION' .
*
*        ME->SIMULASYON( ).         .
**
*    ENDCASE.

  ENDMETHOD.


  METHOD zbc_000_if01~start_of_selection.

    IF mv_submit NE abap_true .

      zbc_000_cl01=>get_selections_from_program(
        EXPORTING
          iv_prog = CONV #( me->mv_report )
        IMPORTING
          es_range = me->ms_range ) .



    check_authorization( ).

    ENDIF.





    me->get_datas( ).

  ENDMETHOD.


  METHOD ZBC_000_IF01~SCREEN_PBO.

    DATA : ls_stable TYPE lvc_s_stbl,
           lv_title  TYPE sy-title.

    SET PF-STATUS 'GUI100' OF PROGRAM iv_program .

    IF zbc_000_if01~go_alv IS INITIAL.
      me->zbc_000_if01~create_alv_object( ).
      me->zbc_000_if01~gv_title = sy-title.
    ELSE.
      ls_stable-row = ls_stable-col = abap_true.
      zbc_000_if01~go_alv->refresh_table_display( EXPORTING is_stable = ls_stable ).
    ENDIF.

*    DESCRIBE TABLE me->mt_alvdat LINES data(lv_count).
*    lv_title = |{ me->zbc_000_if01~gv_title }({ lv_count })|.
    SET TITLEBAR  'TIT100' OF PROGRAM iv_program WITH lv_title.

  ENDMETHOD.


  METHOD zbc_000_if01~get_datas.


    me->select_data( ) .

  ENDMETHOD.


  METHOD ZBC_000_IF01~FILL_LAYOUT.

    rs_layout-zebra                = abap_true.
    rs_layout-cwidth_opt           = abap_true.
    rs_layout-no_rowins            = abap_true.
*    rs_layout-sel_mode             = 'A'.
  ENDMETHOD.


  METHOD ZBC_000_IF01~FILL_FIELD_CATALOG.

    DATA: lv_ddtext TYPE dd03t-ddtext.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = me->zbc_000_if01~gv_struc_name
      CHANGING
        ct_fieldcat            = rt_fcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.



    LOOP AT rt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).

*      CASE <ls_fcat>-fieldname.
*        WHEN MC_FIELDNAME_KEY OR MC_FIELDNAME_XFELD OR MC_FIELDNAME_MANDT OR MC_FIELDNAME_GJAHR_TAH
*          OR MC_FIELDNAME_STBLG_TAH OR MC_FIELDNAME_STBLG.
*          <ls_fcat>-tech   = abap_true.
**        WHEN 'KUNNR' OR 'VALDT'.
**          <ls_fcat>-edit   = abap_true.
*        WHEN MC_FIELDNAME_TKSSY.
*          <ls_fcat>-do_sum = abap_true.
*
*           .
*      ENDCASE.

    ENDLOOP.

 delete rt_fcat where fieldname = ZFI_000_CL02=>MC_FIELDNAME_SELKZ.

  ENDMETHOD.


  METHOD ZBC_000_IF01~EXCLUDE_BUTTONS.

*-&Exculude butons->
    rt_exc_buttons = VALUE #( BASE rt_exc_buttons
                                ( cl_gui_alv_grid=>mc_fc_graph )
                                ( cl_gui_alv_grid=>mc_fc_info )
                                ( cl_gui_alv_grid=>mc_fc_refresh ) ).

  ENDMETHOD.


  METHOD ZBC_000_IF01~END_OF_SELECTION.
*    IF ms_range-pbloke EQ 'X'.
*      musterilere_bloke_koy( ).
*    ENDIF.

*    IF ms_range-p_art IS NOT INITIAL.
*      me->denklestir( ) .
*    ENDIF.
  ENDMETHOD.


  METHOD ZBC_000_IF01~AT_SELECTION_SCREEN.

  ENDMETHOD.


  METHOD ZBC_000_IF01~ALV_USER_COMMAND.
    DATA:
      lt_rows TYPE lvc_t_row,
      ls_rows TYPE lvc_s_row.


    CLEAR: lt_rows, ls_rows .

    CASE e_ucomm.
      WHEN ZFI_000_CL02=>MC_E_UCOMM_DENKLESTIR.
*        CALL METHOD me->zbc_000_if01~go_alv->get_selected_rows(
*          IMPORTING
*            et_index_rows = lt_rows ).

*        CLEAR: mt_alv_selected, wa_alv .
*
*        LOOP AT lt_rows INTO ls_rows.
*          READ TABLE mt_alv INTO wa_alv INDEX ls_rows-index.
*          IF sy-subrc IS INITIAL.
*            APPEND wa_alv TO mt_alv_selected.
*          ENDIF.
*        ENDLOOP.
*        CLEAR: wa_alv.
*      if ms_range-p_rb1 is not INITIAL.
*        me->denklestir( ).
*      elseif ms_range-p_rb2 is NOT INITIAL.
*        me->denklestir_sat( ).
*      endif.
*        me->denklestir( ).
    ENDCASE.
    CALL METHOD me->zbc_000_if01~go_alv->refresh_table_display.
  ENDMETHOD.


  METHOD ZBC_000_IF01~ALV_TOOLBAR.
    DATA: ls_toolbar TYPE stb_button.

    CLEAR ls_toolbar.
    ls_toolbar-butn_type = ZFI_000_CL02=>MC_BUTN_TYPE_3   . " seperator
    APPEND ls_toolbar   TO e_object->mt_toolbar.

*    CLEAR ls_toolbar.
*    ls_toolbar-function  = MC_FUNCTION_DENKLESTIR.
*    ls_toolbar-icon      = ICON_EXECUTE_OBJECT.
*    ls_toolbar-text      = TEXT-b01 .
*    ls_toolbar-quickinfo = TEXT-b01 .
*    APPEND ls_toolbar TO e_object->mt_toolbar.
  ENDMETHOD.


  METHOD ZBC_000_IF01~ALV_MENU_BUTTON.
  ENDMETHOD.


  METHOD ZBC_000_IF01~ALV_HOTSPOT_CLICK.

  ENDMETHOD.


  method ZBC_000_IF01~ALV_DATA_CHANGED_FINISHED.
  endmethod.


  method ZBC_000_IF01~ALV_BUTTON_CLICK.
  endmethod.


  METHOD TOP_OF_PAGE.
    DATA : dl_text(255) TYPE c,
           lo_table     TYPE REF TO cl_dd_table_element,
           lo_qtable    TYPE REF TO cl_dd_table_area.

    CALL METHOD co_dyndoc_id->add_text
      EXPORTING
        text         = TEXT-t03 "'Seçim Parametreleri'
        sap_emphasis = cl_dd_area=>strong.
    "      sap_style    = cl_dd_area=>list_heading.

    CALL METHOD co_dyndoc_id->underline.

* Create table
    CALL METHOD co_dyndoc_id->add_table
      EXPORTING
        no_of_columns               = 3
        cell_background_transparent = ' '
      IMPORTING
        table                       = lo_table
        tablearea                   = lo_qtable.

* Table is only used to set column styles
    CALL METHOD lo_table->set_column_style
      EXPORTING
        col_no    = 1
        sap_style = ZFI_000_CL02=>MC_SAP_STYLE_KEY.


* seçim parametrelerini belirliyoruz.
    DATA: lt_tab     TYPE TABLE OF textpool,
          fname(15),
          value(255).
    FIELD-SYMBOLS: <fs> TYPE any.

    READ TEXTPOOL sy-repid INTO lt_tab LANGUAGE sy-langu.
    LOOP AT lt_tab REFERENCE INTO DATA(lr_tab) WHERE id     EQ ZFI_000_CL02=>MC_ID_S.
*                                                 AND key(1) EQ 'S'.
      value = lr_tab->entry+8.

      CALL METHOD lo_qtable->add_text
        EXPORTING
          text = value.

      CLEAR fname.
      CONCATENATE lr_tab->key ZFI_000_CL02=>MC_KEY_LOW INTO fname.
      ASSIGN (fname) TO <fs>.
      IF <fs> IS ASSIGNED.
        value = <fs>.
        CALL METHOD lo_qtable->add_text
          EXPORTING
            text = value.

        CLEAR fname.
        CONCATENATE lr_tab->key ZFI_000_CL02=>MC_KEY_HIGH INTO fname.
        ASSIGN (fname) TO <fs>.
        value = <fs>.
        CALL METHOD lo_qtable->add_text
          EXPORTING
            text = value.
      ELSE.
        CLEAR fname.
        CONCATENATE lr_tab->key space INTO fname.
        ASSIGN (fname) TO <fs>.
        value = <fs>.
        value = <fs>.
        CALL METHOD lo_qtable->add_text
          EXPORTING
            text = value.
      ENDIF.
      UNASSIGN <fs>.
      CALL METHOD lo_qtable->new_row.
    ENDLOOP.

    CLEAR : dl_text.
    "'Kullanıcı :'
    CONCATENATE TEXT-t04 sy-uname INTO dl_text SEPARATED BY space.

* Adding text
    CALL METHOD co_dyndoc_id->add_text
      EXPORTING
        text         = dl_text
        sap_emphasis = cl_dd_area=>heading.

    CALL METHOD co_dyndoc_id->new_line.

    CLEAR : dl_text.
    WRITE sy-datum TO dl_text.
    "'Tarih     :'
    CONCATENATE TEXT-t05 dl_text INTO dl_text SEPARATED BY space.
* Adding text
    CALL METHOD co_dyndoc_id->add_text
      EXPORTING
        text         = dl_text
        sap_emphasis = cl_dd_area=>heading.
    CALL METHOD co_dyndoc_id->new_line.

    CLEAR : dl_text.
    WRITE sy-uzeit TO dl_text.
    "'Saat      :'
    CONCATENATE TEXT-t06 dl_text INTO dl_text SEPARATED BY space.
* Adding text
    CALL METHOD co_dyndoc_id->add_text
      EXPORTING
        text         = dl_text
        sap_emphasis = cl_dd_area=>heading.

    CALL METHOD co_dyndoc_id->new_line.

  ENDMETHOD.


  METHOD select_data.

    IF ms_range-r_up EQ abap_true.
      me->up( ) .
    ELSEIF ms_range-r_bp EQ abap_true.
      me->bp( ) .
    ELSEIF ms_range-r_ap EQ abap_true.
      me->ap( ) .
    ENDIF.

*    IF mv_vergi IS NOT INITIAL.

* mt_sd - > sd den gelen cariyi mt_sd tablosuna

      DATA : ls_mt_alv2 TYPE zfi_037_s05 .
      DATA: sd_kunnr TYPE kunnr.
      TYPES : BEGIN OF ty_stcd2 ,
                stcd2 TYPE kna1-stcd2,
                kunnr TYPE kna1-kunnr,
              END OF ty_stcd2 .
      DATA : lt_stcd2 TYPE TABLE OF ty_stcd2 .
      DATA : ls_stcd2 LIKE LINE OF lt_stcd2 .

      CLEAR : ls_mt_alv2 , lt_stcd2 , ls_stcd2 .

      clear sd_kunnr.
      LOOP AT ms_range-s_part INTO DATA(r_part).

        sd_kunnr = r_part-low .

      ENDLOOP.

      LOOP AT mt_alv INTO DATA(ls_mt_alv) WHERE stcd2 IS NOT INITIAL .
        ls_stcd2-stcd2 = ls_mt_alv-stcd2 .
        ls_stcd2-kunnr = sd_kunnr .
        COLLECT  ls_stcd2 INTO lt_stcd2 .
      ENDLOOP .
      SORT lt_stcd2 BY stcd2 kunnr .
      DELETE ADJACENT DUPLICATES FROM lt_stcd2 COMPARING stcd2 .


      LOOP AT mt_alv INTO ls_mt_alv .

        MOVE-CORRESPONDING ls_mt_alv TO ls_mt_alv2 .

        IF ls_mt_alv-stcd2 IS INITIAL .
          APPEND ls_mt_alv2 TO mt_alv2 .
        ELSE .
          CLEAR : ls_stcd2 .
          READ TABLE lt_stcd2 INTO ls_stcd2
                WITH KEY stcd2 = ls_mt_alv-stcd2
                  BINARY SEARCH .
          IF sy-subrc IS INITIAL .
            ls_mt_alv2-kunnr = ls_stcd2-kunnr .
          ENDIF .
          COLLECT ls_mt_alv2 INTO mt_alv2 .
        ENDIF .

      ENDLOOP .


    loop at mt_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) .
      read table mt_alv2 into ls_mt_alv2
                    with key stcd2 = <fs_alv>-stcd2 .
      if sy-subrc is INITIAL .
        <fs_alv>-t_KUL_LIMIT = ls_mt_alv2-KUL_LIMIT.
      endif .
    endloop .

*    ENDIF.
  ENDMETHOD.


  METHOD INITIALIZATION.

  ENDMETHOD.


  METHOD GET_INSTANCE.

    IF mo_singleton IS INITIAL.
      CREATE OBJECT mo_singleton.
    ENDIF.
    ro_result = mo_singleton.

  ENDMETHOD.


  METHOD CONSTRUCTOR.

*-&Set value->
*    me->gv_struc_name = mc_cons-strnam.

    me->zbc_000_if01~gv_struc_name = ZFI_000_CL02=>mc_gv_struc_name_ZFI_037_S02.
    CREATE OBJECT zbc_000_if01~go_log .

    zbc_000_if01~go_log->open_log( ).

    MV_VERGI = IV_VERGI .

  ENDMETHOD.


  METHOD check_authorization.

    zfi_000_cl03=>check_authority_f_bkpf_buk(
   EXPORTING
     iv_actvt      = zfi_000_cl02=>mc_actvt_03         " Aktivite
*    iv_bukrs      = iv_bukrs
     iv_no_message = abap_true " Tek basamaklı gösterge
   CHANGING
     ct_bukrs      = me->ms_range-s_bukrs[]
 ).
  ENDMETHOD.
ENDCLASS.
