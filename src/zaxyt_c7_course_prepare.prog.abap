*&---------------------------------------------------------------------*
*& Report zaxyt_c7_course_prepare
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zaxyt_c7_course_prepare.
TABLES: vbak,vbap.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-b01.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_salv RADIOBUTTON GROUP r1 USER-COMMAND rd1.
SELECTION-SCREEN COMMENT 3(20) TEXT-r10 FOR FIELD p_salv.
PARAMETERS p_alv RADIOBUTTON GROUP r1.
SELECTION-SCREEN COMMENT 30(20) TEXT-r11 FOR FIELD p_alv.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-b02.
SELECT-OPTIONS:
  s_vbeln FOR vbak-vbeln,
  s_auart FOR vbak-auart,
  s_kunnr FOR vbak-kunnr,
  s_erdat FOR vbak-erdat.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-b03.
SELECT-OPTIONS:
  s_matnr FOR vbap-matnr.
SELECTION-SCREEN END OF BLOCK b3.

**********************************************************************
* MODEL - 模型定义
**********************************************************************
CLASS lcl_report_model DEFINITION ABSTRACT.
  PUBLIC SECTION.
    TYPES:BEGIN OF ty_flat,
            vbeln  TYPE vbeln,
            erdat  TYPE erdat,
            posnr  TYPE posnr,
            matnr  TYPE matnr,
            maktx  TYPE char40,
            kwmeng TYPE kwmeng,
            vrkme  TYPE vrkme,
            netwr  TYPE netwr,
            waerk  TYPE waerk,
            auart  TYPE auart,
            kunnr  TYPE kunnr,
            name1  TYPE name1,
          END OF ty_flat,
          BEGIN OF ty_header,
            vbeln TYPE vbeln,
            erdat TYPE erdat,
            auart TYPE auart,
            kunnr TYPE kunnr,
            name1 TYPE name1,
          END OF ty_header,
          BEGIN OF ty_item,
            so_num TYPE vbeln,
            posnr  TYPE posnr,
            matnr  TYPE matnr,
            maktx  TYPE char40,
            kwmeng TYPE kwmeng,
            vrkme  TYPE vrkme,
            netwr  TYPE netwr,
            waerk  TYPE waerk,
          END OF ty_item,
          tty_header TYPE TABLE OF ty_header,
          tty_item   TYPE TABLE OF ty_item,
          ty_vbeln   TYPE RANGE OF vbeln,
          ty_auart   TYPE RANGE OF auart,
          ty_kunnr   TYPE RANGE OF kunnr,
          ty_matnr   TYPE RANGE OF matnr,
          ty_erdat   TYPE RANGE OF erdat.
    DATA: alv_data TYPE TABLE OF ty_flat.
    DATA: salv_header TYPE TABLE OF ty_header,
          salv_item   TYPE TABLE OF ty_item.
    METHODS:
      get_data ABSTRACT IMPORTING
                          im_vbeln TYPE ty_vbeln
                          im_auart TYPE ty_auart
                          im_kunnr TYPE ty_kunnr
                          im_matnr TYPE ty_matnr
                          im_erdat TYPE ty_erdat
                        .
ENDCLASS.

CLASS lcl_report_model_alv DEFINITION INHERITING FROM lcl_report_model FINAL.
  PUBLIC SECTION.
    METHODS: get_data REDEFINITION.
ENDCLASS.

CLASS lcl_report_model_salv DEFINITION INHERITING FROM lcl_report_model FINAL.
  PUBLIC SECTION.
    METHODS:
      get_data REDEFINITION.
ENDCLASS.

**********************************************************************
* VIEW - 视图定义
**********************************************************************
CLASS lcl_report_view DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS:
      set_model ABSTRACT IMPORTING ref_model TYPE REF TO lcl_report_model,
      display_data ABSTRACT.
  PROTECTED SECTION.
    DATA: lo_model TYPE REF TO lcl_report_model.
ENDCLASS.

CLASS lcl_report_view_alv DEFINITION INHERITING FROM lcl_report_view FINAL.
  PUBLIC SECTION.
    METHODS:
      display_data REDEFINITION,
      set_model REDEFINITION.
ENDCLASS.

CLASS lcl_report_view_salv DEFINITION INHERITING FROM lcl_report_view FINAL.
  PUBLIC SECTION.
    METHODS:
      display_data REDEFINITION,
      set_model REDEFINITION.
ENDCLASS.

**********************************************************************
* CONTROLLER -  控制器定义
**********************************************************************
CLASS lcl_report_controller DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING
                    im_r_type TYPE char1,
      execute_report
        IMPORTING im_vbeln TYPE lcl_report_model=>ty_vbeln
                  im_auart TYPE lcl_report_model=>ty_auart
                  im_kunnr TYPE lcl_report_model=>ty_kunnr
                  im_matnr TYPE lcl_report_model=>ty_matnr
                  im_erdat TYPE lcl_report_model=>ty_erdat,
      set_report_type IMPORTING r_type TYPE char1.
  PRIVATE SECTION.
    DATA: report_type TYPE char1,
          lo_model    TYPE REF TO lcl_report_model,
          lo_view     TYPE REF TO lcl_report_view.
ENDCLASS.

**********************************************************************
* 主程序执行
**********************************************************************
START-OF-SELECTION.
  DATA(o_controller) = NEW lcl_report_controller( im_r_type = p_alv ).
  o_controller->execute_report(
  EXPORTING
    im_vbeln = s_vbeln[]
    im_auart = s_auart[]
    im_kunnr = s_kunnr[]
    im_matnr = s_matnr[]
    im_erdat = s_erdat[]
  ).

**********************************************************************
* CONTROLLER -  控制器实施
**********************************************************************
CLASS lcl_report_controller IMPLEMENTATION.
  METHOD constructor.
    report_type = SWITCH #( im_r_type
        WHEN 'X' THEN 'A'
        ELSE 'S'
     ).
  ENDMETHOD.
  METHOD set_report_type.
    report_type = r_type.
  ENDMETHOD.
  METHOD execute_report.
*   根据选择，决定报表的模型和视图
    lo_model = SWITCH #( report_type
      WHEN 'A' THEN NEW lcl_report_model_alv( )
      ELSE NEW lcl_report_model_salv( )
    ).

    lo_view = SWITCH #( report_type
      WHEN 'A' THEN NEW lcl_report_view_alv( )
      ELSE NEW lcl_report_view_salv( )
    ).
*   取得数据
    lo_model->get_data(
      EXPORTING
        im_vbeln = im_vbeln
        im_auart = im_auart
        im_kunnr = im_kunnr
        im_matnr = im_matnr
        im_erdat = im_erdat
      ).
*   设置视图的绑定模型
    lo_view->set_model( lo_model ).
*    显示数据
    lo_view->display_data( ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
* MODEL - 模型实施
**********************************************************************
CLASS lcl_report_model_alv IMPLEMENTATION.
  METHOD get_data.
    SELECT
      a~vbeln
      a~erdat
      b~posnr
      b~matnr
      d~maktx
      b~kwmeng
      b~vrkme
      b~netwr
      b~waerk
      a~auart
      a~kunnr
      c~name1
      FROM vbak AS a INNER JOIN vbap AS b
      ON a~vbeln = b~vbeln
      INNER JOIN kna1 AS c
      ON a~kunnr = c~kunnr
      INNER JOIN makt AS d
      ON b~matnr = d~matnr
      AND d~spras = 'E'
      INTO CORRESPONDING FIELDS OF TABLE alv_data
      UP TO 20 ROWS
      WHERE a~vbeln IN im_vbeln
      AND a~auart IN im_auart
      AND a~kunnr IN im_kunnr
      AND b~matnr IN im_matnr
      AND a~erdat IN im_erdat
      .
  ENDMETHOD.
ENDCLASS.

CLASS lcl_report_model_salv IMPLEMENTATION.
  METHOD get_data.
    SELECT
      a~vbeln
      a~erdat
      b~posnr
      b~matnr
      d~maktx
      b~kwmeng
      b~vrkme
      b~netwr
      b~waerk
      a~auart
      a~kunnr
      c~name1
      FROM vbak AS a INNER JOIN vbap AS b
      ON a~vbeln = b~vbeln
      INNER JOIN kna1 AS c
      ON a~kunnr = c~kunnr
      INNER JOIN makt AS d
      ON b~matnr = d~matnr
      AND d~spras = 'E'
      INTO CORRESPONDING FIELDS OF TABLE alv_data
      UP TO 20 ROWS
      WHERE a~vbeln IN im_vbeln
      AND a~auart IN im_auart
      AND a~kunnr IN im_kunnr
      AND b~matnr IN im_matnr
      AND a~erdat IN im_erdat
      .
    salv_header = CORRESPONDING #( alv_data ).
    salv_item = CORRESPONDING #( alv_data MAPPING so_num = vbeln ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
* VIEW - 视图实施
**********************************************************************
CLASS lcl_report_view_alv IMPLEMENTATION.
  METHOD set_model.
    lo_model = ref_model.
  ENDMETHOD.
  METHOD display_data.
    cl_salv_table=>factory(
      EXPORTING
      list_display   = if_salv_c_bool_sap=>false    " ALV Displayed in List Mode
      IMPORTING
      r_salv_table   = DATA(o_salv)    " Basis Class Simple ALV Tables
      CHANGING
      t_table        = lo_model->alv_data
    ).
    o_salv->get_functions( )->set_default(
*        value = IF_SALV_C_BOOL_SAP=>TRUE
    ).
    o_salv->display( ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_report_view_salv IMPLEMENTATION.
  METHOD set_model.
    lo_model = CAST lcl_report_model_salv( ref_model ).
  ENDMETHOD.
  METHOD display_data.
    DATA(lt_binding) = VALUE salv_t_hierseq_binding(
      ( master = 'VBELN' slave = 'SO_NUM' )
    ).

    cl_salv_hierseq_table=>factory(
      EXPORTING
      t_binding_level1_level2 = lt_binding
      IMPORTING
      r_hierseq               = DATA(o_salv_hier)
      CHANGING
      t_table_level1           = lo_model->salv_header
      t_table_level2           = lo_model->salv_item ).
    o_salv_hier->get_functions( )->set_all( ).
    o_salv_hier->display( ).
  ENDMETHOD.
ENDCLASS.
