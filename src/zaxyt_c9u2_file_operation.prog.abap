*&---------------------------------------------------------------------*
*& Report zaxyt_c9u2_file_operation
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zaxyt_c9u2_file_operation.

* 创建选择屏幕
*SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: rad_file TYPE c DEFAULT 'X' RADIOBUTTON GROUP rad USER-COMMAND r1.
    PARAMETERS: rad_bach TYPE c RADIOBUTTON GROUP rad.
*SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.

PARAMETERS:
    p_file TYPE string VISIBLE LENGTH 50,
    p_batch TYPE string VISIBLE LENGTH 50.

**********************************************************************
* 创建 Model 类
**********************************************************************
CLASS lcl_model_data DEFINITION.
PUBLIC SECTION.
    TYPES: BEGIN OF ty_result,
                vbeln TYPE vbeln,
                ernam TYPE ername,
           END OF ty_result.
   CLASS-DATA: it_result TYPE TABLE OF ty_result.
ENDCLASS.

CLASS lcl_model_data IMPLEMENTATION.

ENDCLASS.

**********************************************************************
* 创建 View 类
**********************************************************************
CLASS lcl_view_result DEFINITION.
PUBLIC SECTION.
    METHODS: display_data.
ENDCLASS.

CLASS lcl_view_result IMPLEMENTATION.

  METHOD display_data.
    cl_salv_table=>factory(
      EXPORTING
        list_display   = if_salv_c_bool_sap=>false    " ALV Displayed in List Mode
      IMPORTING
        r_salv_table   = DATA(o_salv)    " Basis Class Simple ALV Tables
      CHANGING
        t_table        = lcl_model_data=>it_result
    ).
    o_salv->get_functions( )->set_default(
*            value = IF_SALV_C_BOOL_SAP=>TRUE
    ).
    o_salv->display( ).
  ENDMETHOD.

ENDCLASS.

**********************************************************************
* 定义控制器类
**********************************************************************
CLASS lcl_main DEFINITION.
PUBLIC SECTION.
    CLASS-METHODS:
        setup_gui_file_search_help,
        setup_batch_file_search_help,
        update_selection_screen.
    METHODS constructor IMPORTING im_type TYPE c
            im_gui_file TYPE string
            im_batch_file TYPE string.
    METHODS:
        excute_report.
PRIVATE SECTION.
    DATA: load_type TYPE c. " G = Gui, B = Batch
    DATA: filename TYPE string.
    DATA: lo_view TYPE REF TO lcl_view_result.
    DATA: lo_model TYPE REF TO lcl_model_data.
    METHODS:
        gui_load,
        batch_load.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.

  METHOD constructor.
     load_type = SWITCH #( im_type
        WHEN 'X' THEN 'G'   " Gui upload
        ELSE 'B' " Batch load
     ).
     filename = SWITCH #( im_type
        WHEN 'X' THEN im_gui_file
        ELSE im_batch_file
     ).
  ENDMETHOD.

  METHOD setup_batch_file_search_help.
    DATA: l_path TYPE dxfields-longpath.
    CHECK rad_bach IS NOT INITIAL.
    CALL FUNCTION 'F4_DXFILENAME_TOPRECURSION'
      EXPORTING
        i_location_flag = '1'    " Flag: Application or presentation server
        i_server        = 'HDB_ER1_01'    " Application Server
        i_path          = '//tmp/'
        filemask        = '*.*'
      IMPORTING
         o_path          = l_path
      .
    SHIFT l_path BY 1 PLACES.
    p_batch = l_path.
  ENDMETHOD.

  METHOD setup_gui_file_search_help.
    DATA: l_file_table TYPE filetable,
          wa_single_file TYPE file_table,
          l_rc TYPE i.
    CHECK rad_file IS NOT INITIAL.
    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        window_title            =  '打开文件'   " Title Of File Open Dialog
      CHANGING
        file_table              =  l_file_table   " Table Holding Selected Files
        rc                      =  l_rc  " Return Code, Number of Files or -1 If Error Occurred
    ).
    IF sy-subrc = 0.
        READ TABLE l_file_table INTO wa_single_file INDEX 1 TRANSPORTING filename.
        p_file = wa_single_file-filename.
    ENDIF.
  ENDMETHOD.

  METHOD update_selection_screen.
    LOOP AT SCREEN.
        IF screen-name = 'P_FILE'.
            screen-input = SWITCH #( rad_file
                WHEN 'X' THEN 1
                ELSE 0
            ).
            MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'P_BATCH'.
            screen-input = SWITCH #( rad_file
                WHEN 'X' THEN 0
                ELSE 1
            ).
            MODIFY SCREEN.
        ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD excute_report.
    lo_view = NEW lcl_view_result( ).
    IF load_type = 'G'.
        gui_load( ).
    ELSE.
        batch_load( ).
    ENDIF.
    lo_view->display_data( ).
  ENDMETHOD.

  METHOD batch_load.
    DATA: wa_file TYPE lcl_model_data=>ty_result.
    DATA: file_string TYPE string.
    TRANSLATE filename TO LOWER CASE.
    OPEN  DATASET filename FOR INPUT IN TEXT MODE  ENCODING DEFAULT.
    DO.
      READ DATASET filename INTO file_string.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      SPLIT file_string AT cl_abap_char_utilities=>horizontal_tab
        INTO wa_file-vbeln wa_file-ernam.
      APPEND wa_file TO lcl_model_data=>it_result.
    ENDDO.

    CLOSE DATASET filename.
  ENDMETHOD.

  METHOD gui_load.
    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = filename    " Name of file
        has_field_separator     = cl_abap_char_utilities=>horizontal_tab
      CHANGING
        data_tab                = lcl_model_data=>it_result    " Transfer table for file contents
    ).
  ENDMETHOD.

ENDCLASS.

**********************************************************************
* 定义搜索帮助
**********************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
    lcl_main=>setup_gui_file_search_help( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_batch.
    lcl_main=>setup_batch_file_search_help( ).

AT SELECTION-SCREEN OUTPUT.
    lcl_main=>update_selection_screen( ).

**********************************************************************
* 主程序
**********************************************************************
START-OF-SELECTION.
DATA(o_report) = NEW lcl_main(
    im_type       = rad_file
    im_gui_file   = p_file
    im_batch_file = p_batch
).

o_report->excute_report( ).
