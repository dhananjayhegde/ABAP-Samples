```ABAP
*&---------------------------------------------------------------------*
*& Report ZDH_FILE_UPLOADER_TEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdh_file_uploader_test.

DATA reader_class TYPE string VALUE 'ZCL_AB_CSV_READER'.

DATA: lo_uploader TYPE REF TO zif_ab_data_reader
      ,lo_csv_uploader TYPE REF TO zcl_ab_csv_reader
      .

TYPES: BEGIN OF ty_input_file,
         lines            TYPE  string,
         city             TYPE  ort01_gp,
         state            TYPE  string,
         country          TYPE  land1_gp,
         uniquename       TYPE  string,
         postalcode       TYPE  pstlz,
         contact          TYPE  string,
         emailaddress     TYPE  string,
         phone            TYPE  string,
         fax              TYPE  string,
         name             TYPE  name1_gp,
         purchasingunit   TYPE  string,
         contactid        TYPE  string,
         parentuniquename TYPE  string,
       END OF ty_input_file.

DATA: lt_input_data     TYPE STANDARD TABLE OF ty_input_file
     ,gt_input_data     TYPE STANDARD TABLE OF ty_input_file
     .

CREATE OBJECT lo_uploader TYPE (reader_class).

lo_uploader->set_header_lines( EXPORTING iv_header_lines = 2 ).
lo_uploader->read_data( CHANGING ct_data = lt_input_data ).

IF lt_input_data IS NOT INITIAL.
  DATA(out) = cl_demo_output=>new( ).
  out->display( lt_input_data ).
ELSE.
  WRITE:/ 'Error'.
ENDIF.

```
