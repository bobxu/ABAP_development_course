*&---------------------------------------------------------------------*
*& Report zaxyt_c8u3_table_control_demo
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
PROGRAM zaxyt_c8u3_table_control_demo.
* 数据定义
DATA: BEGIN OF wa_tab,
        vbeln TYPE vbeln,
        erdat TYPE erdat,
      END OF wa_tab.
DATA: it_tab LIKE TABLE OF wa_tab.

CONTROLS table_control TYPE TABLEVIEW USING SCREEN 9000.

DATA: ok_code TYPE sy-ucomm,
      save_ok TYPE sy-ucomm.
DATA: flag.

*&---------------------------------------------------------------------*
*& Module status_9000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
 SET PF-STATUS 'STAT_9000'.
* SET TITLEBAR 'xxx'.
 IF flag IS INITIAL.
 SELECT vbeln erdat FROM vbak
   INTO CORRESPONDING FIELDS OF table it_tab
   UP TO 20 ROWS.

   DESCRIBE TABLE it_tab LINES table_control-lines.
   flag = 'X'.
 ENDIF.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  save_ok = ok_code.
  CLEAR ok_code.
  IF save_ok = 'ZEXIT'.
    LEAVE PROGRAM.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module TABLE_CONTROL_SETUP OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE table_control_setup OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.
data: test type c.
ENDMODULE.
