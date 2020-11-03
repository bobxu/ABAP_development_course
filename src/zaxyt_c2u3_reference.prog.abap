*&---------------------------------------------------------------------*
*& Report zaxyt_c2u3_reference
*&---------------------------------------------------------------------*
*&  课程：ABAP 开发入门
*&  章节：第二单元第三课时
*&  作者：A_216 @ Aug 20, 2018
*&---------------------------------------------------------------------*
REPORT zaxyt_c2u3_reference.
* Reference 的使用
DATA: ref TYPE REF TO char20.
DATA: ls(20) TYPE c.
ls = 'Hello'.
GET REFERENCE OF ls INTO ref.
WRITE: /, ref->*.

TYPES:
  BEGIN OF ty_tab,
    col1 TYPE char20,
    col2 TYPE char10,
  END OF ty_tab.

DATA: ref1 TYPE REF TO ty_tab.
DATA: itab TYPE STANDARD TABLE OF ty_tab.
DATA: wa TYPE ty_tab.

wa-col1 = 'AAA'.
wa-col2 = 'BBB'.
APPEND wa TO itab.

LOOP AT itab REFERENCE INTO ref1 WHERE col1 = 'AAA'.
  ref1->col2 = 'ZZZ'.
ENDLOOP.

LOOP AT itab INTO wa.
  WRITE: /, wa-col1,wa-col2.
ENDLOOP.
