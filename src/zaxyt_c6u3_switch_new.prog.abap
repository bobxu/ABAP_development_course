*&---------------------------------------------------------------------*
*& Report zaxyt_c6u3_switch_old
*&---------------------------------------------------------------------*
*&  课程：ABAP 开发入门
*&  章节：第六单元第三课时
*&  作者：A_216 @ Aug 20, 2018
*&---------------------------------------------------------------------*
REPORT zaxyt_c6u3_switch_new.

PARAMETERS: p_date LIKE sy-datum.
DATA: l_indicator LIKE scal-indicator,
      l_day       TYPE char10.

CALL FUNCTION 'DATE_COMPUTE_DAY'
  EXPORTING
    date = p_date
  IMPORTING
    day  = l_indicator.

l_day = SWITCH #( l_indicator
  WHEN 1 THEN 'Monday'(326)
  WHEN 2 THEN 'Tuesday'(327)
  WHEN 3 THEN 'Wednesday'(328)
  WHEN 4 THEN 'Thursday'(329)
  WHEN 5 THEN 'Friday'(330)
  WHEN 6 THEN 'Saturday'(331)
  WHEN 7 THEN 'Sunday'(332)
  ELSE 'nothing'
).

WRITE: l_day.
