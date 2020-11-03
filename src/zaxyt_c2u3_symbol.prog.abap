*&---------------------------------------------------------------------*
*& Report zaxyt_c2u3_symbol
*&---------------------------------------------------------------------*
*&  课程：ABAP 开发入门
*&  章节：第二单元第三课时
*&  作者：A_216 @ Aug 20, 2018
*&---------------------------------------------------------------------*
REPORT zaxyt_c2u3_symbol.
* Field-symbol 的使用场景

TYPES:BEGIN OF ty_order,
        ordernumber      TYPE vbeln,
        ordertype        TYPE char10,
        orderdescription TYPE string,
      END OF ty_order.
DATA: wa_order TYPE ty_order,
      it_order TYPE TABLE OF ty_order.
FIELD-SYMBOLS: <fs_it> TYPE ty_order.
* 赋值数据给结构，并且添加到内表
wa_order-ordernumber = '12345'.
wa_order-orderdescription = 'text 1'.
wa_order-ordertype = 'UB'.
APPEND wa_order TO it_order.

wa_order-ordernumber = '67890'.
wa_order-orderdescription = 'text 2'.
wa_order-ordertype = 'ZB'.
APPEND wa_order TO it_order.

READ TABLE it_order INDEX 1 ASSIGNING <fs_it>.
<fs_it>-orderdescription = 'New text 1'.
LOOP AT it_order INTO wa_order.
  WRITE: /, wa_order-ordernumber,wa_order-orderdescription.
ENDLOOP.

DATA: name      TYPE c LENGTH 10,
      field(10).
FIELD-SYMBOLS: <fs>.
name = 'AOXIANGYUNTIAN'.
WRITE: /, name.
field = 'NAME'.
ASSIGN (field) TO <fs>.
<fs> = 'ABCD'.
WRITE: /, name.
