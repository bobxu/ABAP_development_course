*&---------------------------------------------------------------------*
*& Modulpool ZAXYT_C8U3_ELEMENTS_DEMO
*&---------------------------------------------------------------------*
*&  课程：ABAP 开发入门
*&  章节：第八章第二课时
*&  作者：A_216 @ Aug 26, 2018
*&---------------------------------------------------------------------*
PROGRAM zaxyt_c8u3_elements_demo.
* 数据声明
TABLES: vbak.

* 数据定义
DATA: rad1 TYPE c,
      rad2 TYPE c,
      check_box TYPE c.

* 屏幕命令定义
DATA: ok_code TYPE sy-ucomm,
      save_ok TYPE sy-ucomm.

*&---------------------------------------------------------------------*
*& Module status_9000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
 SET PF-STATUS 'STAT_9000'.
 SET TITLEBAR 'TIT'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
    save_ok = ok_code.
    CLEAR ok_code.

    CASE save_ok.
        WHEN 'ZFIND'.
            IF vbak-vbeln IS NOT INITIAL.
                SELECT SINGLE erdat
                FROM vbak
                INTO @vbak-erdat
                WHERE vbeln = @vbak-vbeln.
            ENDIF.
       WHEN 'ZBACK'.
            LEAVE PROGRAM.
       WHEN 'ZRAD'.
            IF rad1 = 'X'.
                CLEAR rad2.
            ELSE.
                CLEAR rad1.
            ENDIF.
       WHEN 'ZCHK'.
            CALL SCREEN 100 STARTING AT 10 10.
            IF check_box = 'X'.
                check_box = 'X'.
            ELSE.
                CLEAR check_box.
            ENDIF.
    ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
 SET PF-STATUS 'STAT_100'.
* SET TITLEBAR 'xxx'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
    save_ok = ok_code.
    CLEAR ok_code.
    IF save_ok = 'ZOK' OR save_ok = 'ZCAN'.
        LEAVE TO SCREEN 0.
    ENDIF.
ENDMODULE.
