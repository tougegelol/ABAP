class ZCL_REPORT definition
  public
  abstract
  create public .

public section.

  interfaces ZIF_SALV .
  interfaces ZIF_REPORT .

  aliases AT_SELECTION_SCREEN
    for ZIF_REPORT~AT_SELECTION_SCREEN .
  aliases AT_SELECTION_SCREEN_OUTPUT
    for ZIF_REPORT~AT_SELECTION_SCREEN_OUTPUT .
  aliases START_OF_SELECTION
    for ZIF_REPORT~START_OF_SELECTION .
protected section.

  aliases R_GUI_ALV_GRID
    for ZIF_SALV~R_GUI_ALV_GRID .
  aliases R_SALV_AGGREGATIONS
    for ZIF_SALV~R_SALV_AGGREGATIONS .
  aliases R_SALV_COLUMN
    for ZIF_SALV~R_SALV_COLUMN .
  aliases R_SALV_COLUMNS_TABLE
    for ZIF_SALV~R_SALV_COLUMNS_TABLE .
  aliases R_SALV_COLUMN_TABLE
    for ZIF_SALV~R_SALV_COLUMN_TABLE .
  aliases R_SALV_DISPLAY_SETTINGS
    for ZIF_SALV~R_SALV_DISPLAY_SETTINGS .
  aliases R_SALV_EVENTS
    for ZIF_SALV~R_SALV_EVENTS .
  aliases R_SALV_EVENTS_TABLE
    for ZIF_SALV~R_SALV_EVENTS_TABLE .
  aliases R_SALV_FILTERS
    for ZIF_SALV~R_SALV_FILTERS .
  aliases R_SALV_FORM_ELEMENT
    for ZIF_SALV~R_SALV_FORM_ELEMENT .
  aliases R_SALV_FORM_LABEL
    for ZIF_SALV~R_SALV_FORM_LABEL .
  aliases R_SALV_FORM_LAYOUT_FLOW
    for ZIF_SALV~R_SALV_FORM_LAYOUT_FLOW .
  aliases R_SALV_FORM_LAYOUT_GRID
    for ZIF_SALV~R_SALV_FORM_LAYOUT_GRID .
  aliases R_SALV_FUNCTIONS_LIST
    for ZIF_SALV~R_SALV_FUNCTIONS_LIST .
  aliases R_SALV_LAYOUT
    for ZIF_SALV~R_SALV_LAYOUT .
  aliases R_SALV_SELECTIONS
    for ZIF_SALV~R_SALV_SELECTIONS .
  aliases R_SALV_SORTS
    for ZIF_SALV~R_SALV_SORTS .
  aliases R_SALV_TABLE
    for ZIF_SALV~R_SALV_TABLE .
  aliases R_TABLE
    for ZIF_SALV~R_TABLE .
  aliases S_DROP
    for ZIF_SALV~S_DROP .
  aliases S_LVC_FCAT
    for ZIF_SALV~S_LVC_FCAT .
  aliases S_LVC_LAYO
    for ZIF_SALV~S_LVC_LAYO .
  aliases S_LVC_ROW
    for ZIF_SALV~S_LVC_ROW .
  aliases S_SALV_LAYOUT_KEY
    for ZIF_SALV~S_SALV_LAYOUT_KEY .
  aliases S_STBL
    for ZIF_SALV~S_STBL .
  aliases S_VARIANT
    for ZIF_SALV~S_VARIANT .
  aliases T_DROP
    for ZIF_SALV~T_DROP .
  aliases T_LVC_FCAT
    for ZIF_SALV~T_LVC_FCAT .
  aliases T_LVC_ROW
    for ZIF_SALV~T_LVC_ROW .
  aliases T_SALV_ROW
    for ZIF_SALV~T_SALV_ROW .

  constants ENABLED type BOOLEAN value `X` ##NO_TEXT.
  constants DISABLED type BOOLEAN value SPACE ##NO_TEXT.
  constants SELECTED type BOOLEAN value `X` ##NO_TEXT.
  constants UNSELECTED type BOOLEAN value SPACE ##NO_TEXT.
  data S_LOG type ZTPUB_002 .
  data:
    BEGIN OF s_config,
      save_log(1)     VALUE enabled,
      display_salv(1) VALUE enabled, "默认的salv显示
      alv_list(1)     VALUE disabled,
      report_name     TYPE syrepid VALUE `SAPLKKBL`,
      gui_status      TYPE sypfkey VALUE `STANDARD_FULLSCREEN`,
      alv_title       TYPE lvc_title,
      list_header     TYPE lvc_title,
    END OF s_config .
  data S_EXTRA_LOG type ZTPUB_003 .

  methods GET_SALV_ATTRIBUTES
    importing
      !IR_SALV_TABLE type ref to CL_SALV_TABLE .
  methods GET_DATA
    raising
      ZCX_RP_MSG .
  methods PROCESS_DATA
    raising
      ZCX_RP_MSG .
  methods DISPLAY_DATA
    raising
      ZCX_RP_MSG .
  methods CHECK_AUTHORITY
    raising
      ZCX_RP_MSG .
  methods ADDED_FUNCTION
    for event ADDED_FUNCTION of CL_SALV_EVENTS
    importing
      !E_SALV_FUNCTION .
  methods DOUBLE_CLICK
    for event DOUBLE_CLICK of CL_SALV_EVENTS_TABLE
    importing
      !ROW
      !COLUMN .
  methods LINK_CLICK
    for event LINK_CLICK of CL_SALV_EVENTS_TABLE
    importing
      !ROW
      !COLUMN .
  methods SET_FIELDS .
  methods SET_SALV_BEFORE_OUTPUT .
  methods SET_SALV_ATTRIBUTES .
  methods SAVE_LOG
    importing
      !PHASE type CLIKE .
  methods EXTRA_MSG
    importing
      value(I_MSG) type CLIKE .
private section.

  methods DISPLAY_VIA_SALV
    importing
      !IR_LIST type ref to DATA .
ENDCLASS.



CLASS ZCL_REPORT IMPLEMENTATION.


  METHOD added_function.


  ENDMETHOD.


  METHOD check_authority.
  ENDMETHOD.


  METHOD display_data.

    display_via_salv( r_table ).

  ENDMETHOD.


  METHOD display_via_salv.

    FIELD-SYMBOLS <lt_list> TYPE table.

    CHECK s_config-display_salv EQ enabled.

    ASSIGN ir_list->* TO <lt_list>.

    IF <lt_list> IS NOT ASSIGNED.
      RETURN.
    ELSE.
      IF s_config-alv_title IS INITIAL.
        s_config-alv_title = |条目数 { lines( <lt_list> ) }|.
      ENDIF.
    ENDIF.

    TRY.
        cl_salv_table=>factory(
          EXPORTING
            list_display = s_config-alv_list
          IMPORTING
            r_salv_table = r_salv_table
          CHANGING
            t_table      = <lt_list> ).
      CATCH cx_salv_msg INTO DATA(lcx_salv_msg).
        MESSAGE lcx_salv_msg->get_text( ) TYPE `E`.
    ENDTRY.

    get_salv_attributes( r_salv_table ).

    set_salv_attributes( ).

    set_fields( ).

    set_salv_before_output( ).

    r_salv_table->display( ).

  ENDMETHOD.


  METHOD double_click.

  ENDMETHOD.


  METHOD extra_msg.

    s_extra_log-num = s_log-num.
    s_extra_log-seqno = s_extra_log-seqno + 1.
    s_extra_log-message = i_msg.

    MODIFY ztpub_003 FROM s_extra_log.

  ENDMETHOD.


  METHOD get_data.
  ENDMETHOD.


  METHOD get_salv_attributes.

    CHECK ir_salv_table IS BOUND.

    r_salv_layout = ir_salv_table->get_layout( ).

    r_salv_selections = ir_salv_table->get_selections( ).

    r_salv_functions_list = ir_salv_table->get_functions( ).

    r_salv_columns_table = ir_salv_table->get_columns( ).

    r_salv_sorts = ir_salv_table->get_sorts( ).

    r_salv_aggregations = ir_salv_table->get_aggregations( ).

    r_salv_filters = ir_salv_table->get_filters( ).

    r_salv_events_table = ir_salv_table->get_event( ).

    r_salv_events ?= r_salv_events_table.

    r_salv_display_settings = ir_salv_table->get_display_settings( ).

    s_salv_layout_key-report = sy-cprog.

    s_stbl-row = `X`.
    s_stbl-col = `X`.

  ENDMETHOD.


  METHOD link_click.

  ENDMETHOD.


  METHOD process_data.
  ENDMETHOD.


  METHOD save_log.

    DATA lv_stamp TYPE timestampl.
    DATA s1 TYPE i.
    DATA s2 TYPE i.

    CHECK s_config-save_log EQ enabled.

    CASE phase.
      WHEN `GET_BEGIN`.

        TRY.
            s_log-num = cl_system_uuid=>create_uuid_c32_static( ).
          CATCH cx_uuid_error.

        ENDTRY.

*        CALL FUNCTION 'NUMBER_GET_NEXT'
*          EXPORTING
*            nr_range_nr             = `01`
*            object                  = `ZLOG_NUM`
*            ignore_buffer           = `X`
*          IMPORTING
*            number                  = s_log-num
*          EXCEPTIONS
*            interval_not_found      = 1
*            number_range_not_intern = 2
*            object_not_found        = 3
*            quantity_is_0           = 4
*            quantity_is_not_1       = 5
*            interval_overflow       = 6
*            buffer_overflow         = 7.
*        IF sy-subrc EQ 0.
          s_log-cprog = sy-cprog.
          s_log-tcode = sy-tcode.
          s_log-uname = sy-uname.
          s_log-batch = sy-batch.
          s_log-slset = sy-slset.

          SELECT SINGLE unam
            INTO s_log-unam
            FROM reposrc
           WHERE progname EQ sy-cprog.

          GET TIME STAMP FIELD lv_stamp.

          CONVERT TIME STAMP lv_stamp TIME ZONE 'UTC+8' INTO DATE s_log-get_begda
                                                             TIME s_log-get_begti.

          s_log-get_stamp = frac( lv_stamp ).

          CALL FUNCTION 'GUI_GET_DESKTOP_INFO'
            EXPORTING
              type   = 5
            CHANGING
              return = s_log-os_name.

          CALL FUNCTION 'TH_USER_INFO'
            IMPORTING
              terminal = s_log-terminal
              addrstr  = s_log-ip.

          MODIFY ztpub_002 FROM s_log.
*        ENDIF.

      WHEN `PROCESS_BEGIN`.

        IF s_log-num IS NOT INITIAL.

          GET TIME STAMP FIELD lv_stamp.

          CONVERT TIME STAMP lv_stamp TIME ZONE 'UTC+8' INTO DATE s_log-process_begda
                                                             TIME s_log-process_begti.

          s_log-process_stamp1 = frac( lv_stamp ).

          s1 = s_log-get_begti+0(2) * 3600
             + s_log-get_begti+2(2) * 60
             + s_log-get_begti+4(2) * 1.

          s2 = s_log-process_begti+0(2) * 3600
             + s_log-process_begti+2(2) * 60
             + s_log-process_begti+4(2) * 1
             + ( s_log-process_begda - s_log-get_begda ) * 86400.

          s_log-get_duration = s2 - s1 + s_log-process_stamp1 - s_log-get_stamp.

          UPDATE ztpub_002
             SET process_begda = s_log-process_begda
                 process_begti = s_log-process_begti
                 process_endda = s_log-process_begda
                 process_endti = s_log-process_begti
                 get_duration  = s_log-get_duration
                 process_stamp1 = s_log-process_stamp1
           WHERE num EQ s_log-num.
        ENDIF.

      WHEN `PROCESS_END`.

        IF s_log-num IS NOT INITIAL.

          GET TIME STAMP FIELD lv_stamp.

          CONVERT TIME STAMP lv_stamp TIME ZONE 'UTC+8' INTO DATE s_log-process_endda
                                                             TIME s_log-process_endti.

          s_log-process_stamp2 = frac( lv_stamp ).

          s1 = s_log-process_begti+0(2) * 3600
             + s_log-process_begti+2(2) * 60
             + s_log-process_begti+4(2) * 1.

          s2 = s_log-process_endti+0(2) * 3600
             + s_log-process_endti+2(2) * 60
             + s_log-process_endti+4(2) * 1
             + ( s_log-process_endda - s_log-process_begda ) * 86400.

          s_log-process_duration = s2 - s1 + s_log-process_stamp2 - s_log-process_stamp1.

          s_log-total_duration = s_log-get_duration + s_log-process_duration.

          UPDATE ztpub_002
             SET process_endda    = s_log-process_endda
                 process_endti    = s_log-process_endti
                 process_duration = s_log-process_duration
                 process_stamp2   = s_log-process_stamp2
                 total_duration   = s_log-total_duration
           WHERE num EQ s_log-num.
        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD set_fields.
  ENDMETHOD.


  METHOD set_salv_attributes.

    r_salv_table->set_screen_status( report   = s_config-report_name
                                     pfstatus = s_config-gui_status ).

    r_salv_layout->set_default( `X` ).

    r_salv_layout->set_save_restriction( 3 ).

    r_salv_layout->set_key( s_salv_layout_key ).

    r_salv_columns_table->set_optimize( `X` ).

    r_salv_functions_list->set_all( `X` ).

    r_salv_selections->set_selection_mode( 4 ).

    CREATE OBJECT r_salv_form_layout_grid.

    r_salv_form_layout_grid->create_label(
      EXPORTING
        row    = 1
        column = 1
        text   = s_config-alv_title ).

    r_salv_table->set_top_of_list( r_salv_form_layout_grid ).

    IF s_config-list_header IS NOT INITIAL.
      r_salv_display_settings->set_list_header( s_config-list_header ).
    ENDIF.

    SET HANDLER added_function FOR r_salv_events.
    SET HANDLER double_click FOR r_salv_events_table.
    SET HANDLER link_click FOR r_salv_events_table.

  ENDMETHOD.


  method SET_SALV_BEFORE_OUTPUT.
  endmethod.


  METHOD zif_report~at_selection_screen.
  ENDMETHOD.


  METHOD zif_report~at_selection_screen_output.
  ENDMETHOD.


  METHOD zif_report~start_of_selection.

    TRY.

        check_authority( ). save_log( `GET_BEGIN` ).

        get_data( ). save_log( `PROCESS_BEGIN` ).

        process_data( ). save_log( `PROCESS_END` ).

        display_data( ).

      CATCH zcx_rp_msg INTO DATA(lcx_rp_msg).

        MESSAGE lcx_rp_msg->get_text( ) TYPE `S` DISPLAY LIKE `E`.

    ENDTRY.

  ENDMETHOD.
ENDCLASS.
