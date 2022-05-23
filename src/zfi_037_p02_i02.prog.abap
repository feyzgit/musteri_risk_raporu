*&---------------------------------------------------------------------*
*& Include          ZFI_070_P01_I02
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Include          ZFI_036_P01_I02
*&---------------------------------------------------------------------*
**********************************************************************
********************--->> LCL_MAIN DEFINITION --->>*******************
**********************************************************************
CLASS lcl_main DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: load_of_program,
      initialization ,
      selection_screen_output ,
      selection_screen ,
      start_of_selection,
      end_of_selection.
ENDCLASS.
**********************************************************************
********************<<--- LCL_MAIN DEFINITION <<---*******************
**********************************************************************
**********************************************************************
********************--->> LCL_MAIN IMPLEMENTATION --->>***************
**********************************************************************
CLASS lcl_main IMPLEMENTATION.
  METHOD load_of_program.
    go_app = ZFI_037_CL02=>get_instance( ).
  ENDMETHOD.
  METHOD initialization.
*    go_app->initialization( CHANGING cv_variant = p_vari ).
  ENDMETHOD.
  METHOD selection_screen_output.
    go_app->at_selection_screen_output( ).
  ENDMETHOD.
  METHOD selection_screen.
    go_app->at_selection_screen(
    EXPORTING
      iv_ucomm = CONV #( sy-ucomm ) ).
  ENDMETHOD.
  METHOD start_of_selection.
    go_app->start_of_selection( ).
  ENDMETHOD.
  METHOD end_of_selection.
*    IF NOT go_app->mv_notfound IS INITIAL.
*      MESSAGE s022(zbc). RETURN.
*    ENDIF.
    go_app->end_of_selection( ).

*    IF p_art IS INITIAL.
      CALL FUNCTION 'ZBC_000_FM01'
        EXPORTING
          io_screen = go_app.
*    ENDIF.

  ENDMETHOD.
ENDCLASS.
**********************************************************************
********************--->> LCL_MAIN IMPLEMENTATION --->>***************
**********************************************************************
