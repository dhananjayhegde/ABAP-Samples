```ABAP
class ZCL_AB_CSV_READER definition
  public
  create public .

public section.

  interfaces ZIF_AB_DATA_READER .
protected section.
private section.
ENDCLASS.



CLASS ZCL_AB_CSV_READER IMPLEMENTATION.


  METHOD zif_ab_data_reader~read_data.
    DATA:  lo_struct        TYPE REF TO cl_abap_structdescr
          ,lo_table         TYPE REF TO cl_abap_tabledescr
          ,lt_comp          TYPE cl_abap_structdescr=>component_table
          ,file_table       TYPE filetable
          ,rc               TYPE i
          ,raw_data_part    TYPE TABLE OF string
          ,raw_data         TYPE TABLE OF string
          ,comp_name        TYPE string
          ,comp_index       TYPE i
          .

*--- 1. Get the type of the input table
    lo_table  ?= cl_abap_structdescr=>describe_by_data( ct_data ).
    lo_struct ?= lo_table->get_table_line_type( ).
    lt_comp    = lo_struct->get_components( ).
*
    DATA(total_components) = lines( lt_comp ).

*--- 2. Load the file from presentation server
    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        window_title            = 'Select a CSV File'
        multiselection          = abap_true
*        initial_directory       = initial_directory
      CHANGING
        file_table              = file_table
        rc                      = rc
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5
           ).

    IF sy-subrc <> 0 OR file_table IS INITIAL.
*      TODO: include proper error handling e.g. propagate the error to caller
      RETURN.
    ENDIF.

    CLEAR: raw_data
          ,raw_data_part
          .

*--- 3. Skip the header rows
    DATA(row_number) = zif_ab_data_reader~mv_header_lines + 1.

*--- 4. Read the files
    DATA sap_codepage  TYPE cpcodepage.
    DATA(codepage) = 'UTF-8'.
    CALL FUNCTION 'SCP_CODEPAGE_BY_EXTERNAL_NAME'
      EXPORTING
        external_name = codepage
        kind          = 'H'
      IMPORTING
        sap_codepage  = sap_codepage
      EXCEPTIONS
        not_found     = 1.

    LOOP AT file_table REFERENCE INTO DATA(lr_file).
      cl_gui_frontend_services=>gui_upload(
          EXPORTING
            filename                = CONV #( lr_file->filename )
            filetype                = 'ASC'
            has_field_separator     = 'X'
            codepage                = CONV #( sap_codepage )
          CHANGING
            data_tab                = raw_data_part
          EXCEPTIONS
            invalid_type  = 1
            OTHERS  = 19
               ).
      IF sy-subrc = 0 AND raw_data_part IS NOT INITIAL.
        raw_data = VALUE #( BASE raw_data
                            ( LINES OF raw_data_part FROM row_number ) ).
        CLEAR raw_data_part.
      ELSEIF sy-subrc <> 0.
*        TODO: implement error hadling
      ENDIF.
    ENDLOOP.

*--- 5. Decode the data from file into input table
    CONSTANTS: c_sep TYPE string VALUE ','
              ,c_delim TYPE string VALUE '"'
              .

    DATA:  offset               TYPE i
          ,start                TYPE i
          ,total_length         TYPE i
          ,is_delim_open        TYPE abap_bool
          ,is_cell_with_delim   TYPE abap_bool
          ,cell_content         TYPE string
          ,cell_content_length  TYPE i
          ,char                 TYPE c LENGTH 1
          .

    DATA: lr_data             TYPE REF TO data.
    FIELD-SYMBOLS: <fs_data>  TYPE any
                  ,<comp>     TYPE any
                  .

    CREATE DATA lr_data TYPE HANDLE lo_struct.

    LOOP AT raw_data REFERENCE INTO DATA(lr_raw_data). "for each row in csv file

      INSERT INITIAL LINE INTO TABLE ct_data REFERENCE INTO lr_data.
      ASSIGN lr_data->* TO <fs_data>.

      offset = 0.
      start  = 0.
      total_length        = strlen( lr_raw_data->* ).
      is_delim_open       = abap_false.
      cell_content        = ''.
      cell_content_length = 0.

      comp_index = 1.

      WHILE offset < total_length.
        char = lr_raw_data->*+offset(1).

        IF char = c_delim.
          IF is_delim_open  = abap_true.
            is_delim_open   = abap_false.
          ELSE.
            is_delim_open       = abap_true.
            is_cell_with_delim  = abap_true.
            start = start + 1. " Do not include delimter in the content, ignore the opening delimiter
          ENDIF.
        ELSE.
          IF char = c_sep AND is_delim_open = abap_false.
            " If separator is found when delimiter is open, then we do not do anything!
            " If separator is found outside the delimiter, then we consider that a column
            " ends there
            IF is_cell_with_delim = abap_true.
              cell_content_length = offset - start - 1. " -1 to ignore the closing delimiter
              is_cell_with_delim  = abap_false.
            ELSE.
              cell_content_length = offset - start.
            ENDIF.
            cell_content = lr_raw_data->*+start(cell_content_length).

            IF comp_index > total_components.
              " TODO: more columns found than expected! Error handling to be done
              EXIT.
            ENDIF.
            ASSIGN COMPONENT comp_index OF STRUCTURE <fs_data> TO <comp>.
            <comp>      = cell_content.
            comp_index  = comp_index + 1.
            start       = offset + 1. " Reset 'start' to next character
          ENDIF.
        ENDIF.
        offset = offset + 1.
      ENDWHILE.
    ENDLOOP.

  ENDMETHOD.


  method ZIF_AB_DATA_READER~GET_HEADER_LINES.
    rv_header_Lines = ZIF_AB_DATA_READER~mv_header_lines.
  endmethod.


  method ZIF_AB_DATA_READER~SET_HEADER_LINES.
    ZIF_AB_DATA_READER~mv_header_lines = iv_header_lines.
  endmethod.
ENDCLASS.
```
