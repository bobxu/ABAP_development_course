*&---------------------------------------------------------------------*
*& Report zaxyt_c6u3_switch_old
*&---------------------------------------------------------------------*
*&  课程：ABAP 开发入门
*&  章节：第六单元第三课时
*&  作者：A_216 @ Aug 20, 2018
*&---------------------------------------------------------------------*
REPORT zaxyt_c6u3_switch_old.

PARAMETERS: p_date LIKE sy-datum.
DATA: l_indicator LIKE scal-indicator,
      l_day       TYPE char10.

CALL FUNCTION 'DATE_COMPUTE_DAY'
  EXPORTING
    date = p_date
  IMPORTING
    day  = l_indicator.

CASE l_indicator.
  WHEN 1.
    l_day = 'Monday'(326).
  WHEN 2.
    l_day = 'Tuesday'(327).
  WHEN 3.
    l_day = 'Wednesday'(328).
  WHEN 4.
    l_day = 'Thursday'(329).
  WHEN 5.
    l_day = 'Friday'(330).
  WHEN 6.
    l_day = 'Saturday'(331).
  WHEN 7.
    l_day = 'Sunday'(332).
  WHEN OTHERS.
    l_day = 'test'.
ENDCASE.

WRITE: l_day.
