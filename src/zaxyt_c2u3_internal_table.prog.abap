*&---------------------------------------------------------------------*
*& Report zaxyt_c2u3_internal_table
*&---------------------------------------------------------------------*
*&  课程：ABAP 开发入门
*&  章节：第二单元第三课时
*&  作者：A_216 @ Aug 20, 2018
*&---------------------------------------------------------------------*
REPORT zaxyt_c2u3_internal_table.

* 内表的定义，操作
TYPES:BEGIN OF ty_order,
        ordernumber      TYPE vbeln,
        ordertype        TYPE char10,
        orderdescription TYPE string,
      END OF ty_order.
DATA: wa_order TYPE ty_order,
      it_order TYPE TABLE OF ty_order.

* 赋值数据给结构，并且添加到内表
wa_order-ordernumber = '12345'.
wa_order-orderdescription = 'text 1'.
wa_order-ordertype = 'UB'.
APPEND wa_order TO it_order.

wa_order-ordernumber = '67890'.
wa_order-orderdescription = 'text 2'.
wa_order-ordertype = 'ZB'.
APPEND wa_order TO it_order.

WRITE: /, '添加记录到内表'.
LOOP AT it_order INTO wa_order.
  WRITE: /, wa_order-ordernumber,wa_order-orderdescription.
ENDLOOP.

WRITE: /, '读取一条记录到工作区'.
READ TABLE it_order INTO wa_order WITH KEY ordernumber = '12345'.
WRITE: /, wa_order-ordernumber,wa_order-orderdescription.

WRITE: /, '修改内表的值'.
READ TABLE it_order INTO wa_order INDEX 2.
wa_order-orderdescription = 'new text 2'.
MODIFY it_order INDEX 2 FROM wa_order .

LOOP AT it_order INTO wa_order.
  WRITE: /, wa_order-ordernumber,wa_order-orderdescription.
ENDLOOP.

WRITE: /, '删除一条记录'.
DELETE it_order INDEX 2.
LOOP AT it_order INTO wa_order.
  WRITE: /, wa_order-ordernumber,wa_order-orderdescription.
ENDLOOP.
