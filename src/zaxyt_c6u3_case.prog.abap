*&---------------------------------------------------------------------*
*& Report zaxyt_c6u3_case
*&---------------------------------------------------------------------*
*&  课程：ABAP 开发入门
*&  章节：第六单元第三课时
*&  作者：A_216 @ Aug 20, 2018
*&---------------------------------------------------------------------*
REPORT zaxyt_c6u3_case.

CONSTANTS: lc_ordtype1 TYPE char10 VALUE '标准订单',
           lc_ordtype2 TYPE char10 VALUE '跨区域订单',
           lc_ordtype3 TYPE char10 VALUE '其他订单'.

SELECT vbeln AS col1, erdat AS col2,
  CASE
  WHEN auart = 'OR' THEN @lc_ordtype1
  WHEN auart = 'ZORC' THEN @lc_ordtype2
  ELSE @lc_ordtype3
  END AS col3
  FROM vbak
  UP TO 30 ROWS
INTO TABLE @DATA(lt_tab).

cl_demo_output=>display( lt_tab[] ).
