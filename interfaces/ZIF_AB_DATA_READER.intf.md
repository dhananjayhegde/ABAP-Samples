```ABAP
interface ZIF_AB_DATA_READER
  public .


  data MV_HEADER_LINES type I .

  methods READ_DATA
    changing
      !CT_DATA type ANY TABLE .
  methods SET_HEADER_LINES
    importing
      !IV_HEADER_LINES type I .
  methods GET_HEADER_LINES
    returning
      value(RV_HEADER_LINES) type I .
endinterface.

```
