class ZFI_037_CL02 definition
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

  data MS_RANGE type ZFI_037_S03 .
  data MT_ALV type ZFI_037_S04_TT01 .
  data MO_GRID type ref to CL_GUI_ALV_GRID .
  data MV_ERROR type CHAR1 .
  data:
    it_bdcdata    TYPE TABLE OF bdcdata .
  data:
    gs_bdcdata    LIKE LINE OF it_bdcdata .
  data:
    it_bdcmsgcoll TYPE TABLE OF bdcmsgcoll .
  data GS_BDCMSGCOLL type BDCMSGCOLL .


  methods CONSTRUCTOR .
  class-methods GET_INSTANCE
    returning
      value(RO_RESULT) type ref to ZFI_037_CL02 .
  methods INITIALIZATION
    changing
      !CV_VARIANT type DISVARIANT-VARIANT .
  methods TOP_OF_PAGE
    changing
      value(CO_DYNDOC_ID) type ref to CL_DD_DOCUMENT optional .
  methods SELECT_DATA .
protected section.
private section.

  class-data MO_SINGLETON type ref to ZFI_037_CL02 .
  data MV_REPORT type RALDB_REPO value 'ZFI_037_P02' ##NO_TEXT.
  data MV_ANY_CLR_DOC type XFELD .
ENDCLASS.



CLASS ZFI_037_CL02 IMPLEMENTATION.


  METHOD ZBC_000_IF01~USER_COMMAND.

*    CASE iv_ucomm                 .
*      WHEN 'SIMULATION' .
*
*        ME->SIMULASYON( ).         .
**
*    ENDCASE.

  ENDMETHOD.


  METHOD ZBC_000_IF01~START_OF_SELECTION.

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


  METHOD ZBC_000_IF01~SCREEN_PAI.

    CASE iv_ucomm.
      WHEN ZFI_000_CL02=>MC_IV_UCOMM_BACK.
        LEAVE TO SCREEN 0.
      WHEN ZFI_000_CL02=>MC_IV_UCOMM_EXIT OR ZFI_000_CL02=>MC_IV_UCOMM_CANCEL.
        LEAVE SCREEN.
    ENDCASE.

  ENDMETHOD.


  METHOD ZBC_000_IF01~GET_DATAS.

    zbc_000_cl01=>get_selections_from_program(
      EXPORTING
        iv_prog = CONV #( me->mv_report )
      IMPORTING
        es_range = me->ms_range ) .

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
*    SET HANDLER me->zbc_000_if01~alv_double_click  FOR zbc_000_if01~go_alv.
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


  METHOD ZBC_000_IF01~AT_SELECTION_SCREEN_OUTPUT.

*-&Read Selection Parameter->
    zbc_000_cl01=>get_selections_from_program(
      EXPORTING
        iv_prog = CONV #( me->mv_report )
      IMPORTING
        es_range = me->ms_range ) .


  ENDMETHOD.


  METHOD ZBC_000_IF01~AT_SELECTION_SCREEN.

  ENDMETHOD.


  METHOD ZBC_000_IF01~ALV_USER_COMMAND.
    DATA:
      lt_rows TYPE lvc_t_row,
      ls_rows TYPE lvc_s_row.


    CLEAR: lt_rows, ls_rows .

*    CASE e_ucomm.
*      WHEN ZFI_000_CL02=>MC_E_UCOMM_DENKLESTIR.
**        CALL METHOD me->zbc_000_if01~go_alv->get_selected_rows(
**          IMPORTING
**            et_index_rows = lt_rows ).
*
**        CLEAR: mt_alv_selected, wa_alv .
**
**        LOOP AT lt_rows INTO ls_rows.
**          READ TABLE mt_alv INTO wa_alv INDEX ls_rows-index.
**          IF sy-subrc IS INITIAL.
**            APPEND wa_alv TO mt_alv_selected.
**          ENDIF.
**        ENDLOOP.
**        CLEAR: wa_alv.
**      if ms_range-p_rb1 is not INITIAL.
**        me->denklestir( ).
**      elseif ms_range-p_rb2 is NOT INITIAL.
**        me->denklestir_sat( ).
**      endif.
**        me->denklestir( ).
*    ENDCASE.
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


  METHOD ZBC_000_IF01~ALV_DOUBLE_CLICK.

  ENDMETHOD.


  method ZBC_000_IF01~ALV_DATA_CHANGED_FINISHED.
  endmethod.


  method ZBC_000_IF01~ALV_DATA_CHANGED.
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

*----------------------------------------*
*     DATA DEFINATIONS
*----------------------------------------*

    DATA: ls_alv LIKE LINE OF mt_alv.
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

    DATA: lt_fatura TYPE TABLE OF bsid.

    DATA lv_fname TYPE char40 VALUE 'RV_ORDER_FLOW_INFORMATION'.
*----------------------------------------*
*     AÇIK KALEMLER
*----------------------------------------*

    SELECT * FROM bsid INTO TABLE @DATA(lt_bsid)
    WHERE bukrs IN @ms_range-s_bukrs AND
          kunnr IN @ms_range-s_part AND
          budat LE @ms_range-p_keydt .

    SELECT * FROM bsad APPENDING TABLE lt_bsid
      WHERE  bukrs IN ms_range-s_bukrs AND
             kunnr IN ms_range-s_part AND
             budat LE ms_range-p_keydt AND
             augdt GT ms_range-p_keydt .

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
*     MÜŞTERİ ÖZET TABLOLARI
*----------------------------------------*

    SELECT * FROM knc1 INTO TABLE @DATA(lt_knc1)
      WHERE bukrs IN @ms_range-s_bukrs AND
            kunnr IN @ms_range-s_part AND
            gjahr EQ @ms_range-p_keydt(4) .

    SELECT * FROM knb4 INTO TABLE @DATA(lt_knb4)
       WHERE bukrs IN @ms_range-s_bukrs AND
        kunnr IN @ms_range-s_part .

*----------------------------------------*
*     ORGANİZASYONEL DATA
*----------------------------------------*

    SELECT * FROM but000 INTO TABLE @DATA(lt_but000)
      WHERE partner IN @ms_range-s_part.

    SELECT * FROM t001  INTO TABLE @DATA(lt_001)
      WHERE bukrs IN @ms_range-s_bukrs.

    SELECT * FROM kna1 INTO TABLE @DATA(lt_kna1)
      WHERE kunnr IN @ms_range-s_part.

    " lt_bkpf tablosu hiç bir yerde kullanılmadığı için kapatılmıştır
    " alan belirtilmedildiği için atc checke takılıyor.
*    IF lt_bsid IS NOT INITIAL.
*      SELECT * FROM bkpf INTO TABLE @DATA(lt_bkpf)
*        FOR ALL ENTRIES IN @lt_bsid
*        WHERE bukrs EQ @lt_bsid-bukrs AND
*              belnr EQ @lt_bsid-belnr AND
*              gjahr EQ @lt_bsid-gjahr.
*    ENDIF.

*----------------------------------------*
*     TOPLAM FATURA
*----------------------------------------*

    SELECT * FROM bsid JOIN bkpf ON
                  bsid~bukrs EQ bkpf~bukrs AND
                  bsid~belnr EQ bkpf~belnr AND
                  bsid~gjahr EQ bkpf~gjahr
             INTO CORRESPONDING FIELDS OF TABLE lt_fatura
    WHERE bsid~bukrs IN ms_range-s_bukrs AND
          bsid~kunnr IN ms_range-s_part AND
          bsid~budat LE ms_range-p_keydt AND
          bkpf~gjahr EQ ms_range-p_keydt(4) AND
          bkpf~xreversal EQ space AND
          bkpf~blart IN ( zfi_000_cl02=>mc_blart_dr , zfi_000_cl02=>mc_blart_rv ) AND
          bsid~umskz EQ space AND
          bsid~shkzg EQ zfi_000_cl02=>mc_shkzg_s.

    SELECT * FROM bsad JOIN bkpf ON
                  bsad~bukrs EQ bkpf~bukrs AND
                  bsad~belnr EQ bkpf~belnr AND
                  bsad~gjahr EQ bkpf~gjahr
         APPENDING CORRESPONDING FIELDS OF TABLE lt_fatura
    WHERE bsad~bukrs IN ms_range-s_bukrs AND
          bsad~kunnr IN ms_range-s_part AND
          bsad~budat LE ms_range-p_keydt AND
          bkpf~gjahr EQ ms_range-p_keydt(4) AND
          bkpf~xreversal EQ space AND
          bkpf~blart IN ( zfi_000_cl02=>mc_blart_dr , zfi_000_cl02=>mc_blart_rv ) AND
          bsad~umskz EQ space AND
          bsad~shkzg EQ zfi_000_cl02=>mc_shkzg_s .

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
    AND vbap~lfsta IN ( @zfi_000_cl02=>mc_lfsta_a, @zfi_000_cl02=>mc_lfsta_b ).


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
           likp~vkorg
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
    AND likp~gbstk IN (@zfi_000_cl02=>mc_gbstk_a, @zfi_000_cl02=>mc_gbstk_b)
    AND likp~vbtyp EQ @zfi_000_cl02=>mc_vbtyp_j.

*----------------------------------------*
*     PROCESS DATA
*----------------------------------------*

    LOOP AT lt_fatura REFERENCE INTO DATA(lr_fatura).

      CALL FUNCTION 'NET_DUE_DATE_GET'
        EXPORTING
          i_zfbdt = lr_fatura->zfbdt
          i_zbd1t = lr_fatura->zbd1t
          i_zbd2t = lr_fatura->zbd2t
          i_zbd3t = lr_fatura->zbd3t
          i_shkzg = lr_fatura->shkzg
          i_rebzg = lr_fatura->rebzg
          i_koart = zfi_000_cl02=>mc_koart_d
        IMPORTING
          e_faedt = lr_fatura->zfbdt.

    ENDLOOP.

    LOOP AT lt_bsid REFERENCE INTO DATA(lr_bsid).

      CLEAR: ls_alv.

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

      MOVE-CORRESPONDING lr_bsid->* TO ls_alv.
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
        lr_bsid->umskz EQ zfi_000_cl02=>mc_umskz_s ).
        IF lr_bsed->wstat = zfi_000_cl02=>mc_wstat_m.
          ADD lr_bsid->dmbtr TO ls_alv-musteri_ceki.
        ELSE.
          ADD lr_bsid->dmbtr TO ls_alv-cirolu_cek.
        ENDIF.

        IF lr_bsid->umskz EQ zfi_000_cl02=>mc_umskz_1 AND
           lr_bsid->zfbdt GT ms_range-p_keydt.
          ADD lr_bsid->dmbtr TO ls_alv-vgckt.
        ELSEIF lr_bsid->umskz EQ zfi_000_cl02=>mc_umskz_s AND
           lr_bsid->zfbdt GT ms_range-p_keydt.
          ADD lr_bsid->dmbtr TO ls_alv-vgsnt.
        ENDIF.

      ENDIF.

      IF lr_bsid->umskz EQ zfi_000_cl02=>mc_umskz_w AND
         lr_bsid->shkzg EQ zfi_000_cl02=>mc_shkzg_h.

        ADD lr_bsid->dmbtr TO ls_alv-kcbky.
        ADD lr_bsid->dmbtr TO ls_alv-kcsat.


      ELSEIF lr_bsid->umskz EQ zfi_000_cl02=>mc_umskz_w AND
             lr_bsid->shkzg EQ zfi_000_cl02=>mc_shkzg_s.

        ADD lr_bsid->dmbtr TO ls_alv-kcbky.
        ADD lr_bsid->dmbtr TO ls_alv-kcsbt.

      ENDIF.

      " şirket kodu para birimi
      READ TABLE lt_001 REFERENCE INTO DATA(lr_001)
      WITH KEY bukrs = lr_bsid->bukrs.
      IF sy-subrc IS INITIAL.
        ls_alv-waers = lr_001->waers.
      ENDIF.

      ls_alv-datum = ms_range-p_keydt .

      COLLECT ls_alv INTO mt_alv.

    ENDLOOP.


    LOOP AT mt_alv REFERENCE INTO DATA(lr_alv).

*----------------------------------------------------*
*              BORÇ ALACAK VE BAKİYE
*----------------------------------------------------*

      READ TABLE lt_knc1 INTO wa_knc1
      WITH KEY kunnr = lr_alv->kunnr
               bukrs = lr_alv->bukrs
               gjahr = ms_range-p_keydt(4) .
      IF sy-subrc IS INITIAL.

        lr_alv->bakiy = wa_knc1-umsav.
        IF wa_knc1-umsav LE 0 .
          lr_alv->alckt = wa_knc1-umsav.
        ELSE.
          lr_alv->borct = wa_knc1-umsav.
        ENDIF.

        DO 16 TIMES VARYING var_soll
                    FROM wa_knc1-um01s NEXT wa_knc1-um02s
                    VARYING var_haben
                    FROM wa_knc1-um01h NEXT wa_knc1-um02h.

          lr_alv->bakiy = lr_alv->bakiy + var_soll - var_haben.
          lr_alv->borct = lr_alv->borct + var_soll.
          lr_alv->alckt = lr_alv->alckt + var_haben.

        ENDDO.

      ENDIF.

*----------------------------------------------------*
*              VADE AŞIMI HESAPLAMA
*----------------------------------------------------*

      CLEAR: var_ag, var_vz, ct, prod, wa_knb4,tarih. "erdal, tarih kısmı sadece.
      WRITE ms_range-p_keydt(4) TO tarih.   "erdal..
      " Müşteriye ilişkin ödeme tarihçesi
      CLEAR: wa_knb4.
      READ TABLE lt_knb4 INTO wa_knb4
      WITH KEY kunnr = lr_alv->kunnr
               bukrs = lr_alv->bukrs .

      IF sy-subrc EQ 0.
        " dolu dönem sayısını buluyoruz.
        ct = 0.
        CLEAR: jahxx, monxx, agsxx, vzsxx, agnxx, vznxx,anzxx.
        DO 16 TIMES VARYING jahxx FROM wa_knb4-jah01
        NEXT wa_knb4-jah02
        VARYING monxx FROM wa_knb4-mon01
        NEXT wa_knb4-mon02
        VARYING agsxx FROM wa_knb4-ags01
        NEXT wa_knb4-ags02
        VARYING vzsxx FROM wa_knb4-vzs01
        NEXT wa_knb4-vzs02
        VARYING agnxx FROM wa_knb4-agn01
        NEXT wa_knb4-agn02
        VARYING vznxx FROM wa_knb4-vzn01
        NEXT wa_knb4-vzn02
        VARYING anzxx FROM wa_knb4-anz01
        NEXT wa_knb4-anz02.
          IF anzxx = 0 . "and wa_knb4-jahxx = tarih     " tarih kısmı sadece
            EXIT.
          ENDIF.

          ADD 1 TO ct.
        ENDDO.
        CLEAR jahxx.
        toplam = 0.
        toplam_tum = 0.
        DO ct     TIMES VARYING var_ag FROM wa_knb4-ags01
        NEXT wa_knb4-ags02
        VARYING jahxx  FROM wa_knb4-jah01
        NEXT wa_knb4-jah02.
          IF jahxx = tarih.
            toplam =  toplam  + var_ag.
          ENDIF.
          toplam_tum =  toplam_tum  + var_ag.
        ENDDO.

        ag_toplam = 0.
        ag_toplam_tum = 0.
        DO ct     TIMES VARYING var_ag FROM wa_knb4-ags01
        NEXT wa_knb4-ags02
        VARYING var_vz FROM wa_knb4-vzs01
        NEXT wa_knb4-vzs02
        VARYING jahxx  FROM wa_knb4-jah01
        NEXT wa_knb4-jah02.
          prod    = var_ag * var_vz.
          IF tarih = jahxx.
            ag_toplam = ag_toplam + prod.
          ENDIF.
          ag_toplam_tum = ag_toplam_tum + prod.
        ENDDO.

        lr_alv->vdasm = ag_toplam / toplam.
        lr_alv->vdasm_tum = ag_toplam_tum / toplam_tum.    "erdal
        lr_alv->ags = toplam.    "erdal
        lr_alv->ags_tum = toplam_tum .    "erdal

      ENDIF.
*----------------------------------------------------*
*              TAHMİNİ VADE AŞIMI
*----------------------------------------------------*

      CLEAR: alacak_toplam , borc_toplam , ag_alacak_toplam,
             ag_borc_toplam .

      LOOP AT lt_bsid REFERENCE INTO lr_bsid
        WHERE bukrs EQ lr_alv->bukrs AND
              kunnr EQ lr_alv->kunnr AND
              umskz EQ space AND
              augbl EQ space AND
              zfbdt LE lr_alv->datum.

        IF lr_bsid->shkzg EQ zfi_000_cl02=>mc_shkzg_h.
          alacak_toplam = alacak_toplam - lr_bsid->dmbtr.
          ag_alacak_toplam = ag_alacak_toplam +
            ( lr_bsid->dmbtr * ( lr_alv->datum - lr_bsid->zfbdt ) * -1 ).
        ELSE.
          borc_toplam = borc_toplam + lr_bsid->dmbtr.
          ag_borc_toplam = ag_borc_toplam +
            ( lr_bsid->dmbtr * ( lr_alv->datum - lr_bsid->zfbdt ) ).
        ENDIF.
      ENDLOOP.
      IF ( borc_toplam + alacak_toplam ) NE 0 .
        lr_alv->tvdas =  +  ( ag_borc_toplam / borc_toplam )
                        -   ( ag_alacak_toplam / alacak_toplam ).
      ENDIF.

*----------------------------------------------------*
*              TOPLAM CİRO ORTALAMA UYGULANAN VADE
*----------------------------------------------------*

      CLEAR: ag_toplam .
      LOOP AT lt_fatura REFERENCE INTO lr_fatura
        WHERE bukrs EQ lr_alv->bukrs AND
              kunnr EQ lr_alv->kunnr.

        ADD lr_fatura->dmbtr TO lr_alv->cirot.
        ag_toplam = ag_toplam + ( lr_fatura->dmbtr
              * ( lr_fatura->zfbdt - lr_fatura->bldat ) ) .
        IF lr_fatura->budat+4(2) EQ ms_range-p_keydt+4(2) .
          ADD lr_fatura->dmbtr TO lr_alv->cirot_ay .
        ENDIF.

      ENDLOOP.
      IF lr_alv->cirot NE 0 .
        lr_alv->ouyvd = ag_toplam / lr_alv->cirot .
      ENDIF.

*----------------------------------------------------*
*              SİPARİŞ TUTARI
*----------------------------------------------------*

      LOOP AT lt_vbap REFERENCE INTO DATA(lr_vbap)
        WHERE bukrs EQ lr_alv->bukrs AND
              kunnr EQ lr_alv->kunnr.

        CHECK lr_vbap->kwmeng GT 0.

        IF lr_vbap->lfsta EQ zfi_000_cl02=>mc_lfsta_b.

          DATA: x_comwa TYPE vbco6.
          DATA: lt_vbfa TYPE TABLE OF vbfa .


          REFRESH: lt_vbfa.
          CLEAR: toplam.

          x_comwa-vbeln = lr_vbap->vbeln.
          x_comwa-posnr = lr_vbap->posnr.

          CALL FUNCTION lv_fname
            EXPORTING
*             AUFBEREITUNG  = '2'
              belegtyp      = zfi_000_cl02=>mc_belegtyp_c
              comwa         = x_comwa
*             NACHFOLGER    = 'X'
*             N_STUFEN      = '50'
*             VORGAENGER    = 'X'
*             V_STUFEN      = '50'
* IMPORTING
*             BELEGTYP_BACK =
            TABLES
              vbfa_tab      = lt_vbfa
            EXCEPTIONS
              no_vbfa       = 1
              no_vbuk_found = 2
              OTHERS        = 3.
          IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
          ELSE.
            LOOP AT lt_vbfa REFERENCE INTO DATA(lr_vbfa)
            WHERE vbelv EQ lr_vbap->vbeln
            AND posnv EQ lr_vbap->posnr
            AND vbtyp_v EQ zfi_000_cl02=>mc_vbtyp_v_c
            AND vbtyp_n EQ zfi_000_cl02=>mc_vbtyp_n_j.

              toplam = toplam + lr_vbfa->rfmng.

            ENDLOOP.
          ENDIF.
        ELSE.
          CLEAR toplam .
        ENDIF.

        CLEAR: tutar_bpb.
        tutar_bpb = ( lr_vbap->netwr  + lr_vbap->mwsbp   ) *
        ( lr_vbap->kwmeng - toplam ) /
        lr_vbap->kwmeng .

        CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
          EXPORTING
*           CLIENT           = SY-MANDT
            date             = ms_range-p_keydt
            foreign_amount   = tutar_bpb
            foreign_currency = lr_vbap->waerk
            local_currency   = lr_alv->waers
*           RATE             = 0
            type_of_rate     = ZFI_000_CL02=>mc_type_of_rate_m
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

        IF lr_vbap->lifsk EQ space.
          ADD tutar_upb TO lr_alv->onlst.
        ELSE.
          ADD tutar_upb TO lr_alv->onzst.
        ENDIF.

      ENDLOOP.

*----------------------------------------------------*
*              TESLİMAT TUTARI
*----------------------------------------------------*

      LOOP AT lt_lips REFERENCE INTO DATA(lr_lips)
        WHERE bukrs EQ lr_alv->bukrs AND
              kunag EQ lr_alv->kunnr.

        CHECK lr_lips->kwmeng GT 0.

        CLEAR: tutar_bpb, tutar_upb .
        tutar_bpb = ( lr_lips->netwr  + lr_lips->mwsbp   )
        / lr_lips->kwmeng * lr_lips->lfimg.

* para birimi cevir
*          PERFORM tutar_cevir USING ptarih
*                tutar_bpb it_lips-waerk
*                p_waers
*          CHANGING tutar_upb.

        CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
          EXPORTING
*           CLIENT           = SY-MANDT
            date             = ms_range-p_keydt
            foreign_amount   = tutar_bpb
            foreign_currency = lr_lips->waerk
            local_currency   = lr_alv->waers
*           RATE             = 0
            type_of_rate     = ZFI_000_CL02=>mc_type_of_rate_m
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
          ADD tutar_upb TO lr_alv->ftrit.
        ELSE.
          ADD tutar_upb TO lr_alv->teslt .

        ENDIF.

      ENDLOOP.
    ENDLOOP.

    SORT mt_alv BY kunnr bukrs .

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

    me->zbc_000_if01~gv_struc_name = ZFI_000_CL02=>mc_gv_struc_name_ZFI_037_S04.
    CREATE OBJECT zbc_000_if01~go_log .

    zbc_000_if01~go_log->open_log( ).

  ENDMETHOD.
ENDCLASS.
