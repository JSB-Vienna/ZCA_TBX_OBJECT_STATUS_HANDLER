"! <p class="shorttext synchronized" lang="en">CA-TBX: BC Object status handler</p>
"!
"! The class can check all status, but changes only user status and only with human
"! readable status !! Therefore the default language key is set in the class constructor
"! to German. It can be change via class method CHANGE_LANGUAGE.
CLASS zcl_ca_obj_status DEFINITION PUBLIC
                                   CREATE PROTECTED.

* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      bi_object,
      bi_persistent,
      if_workflow,
      if_xo_const_message.

*   a l i a s e s
    ALIASES:
*     BI_OBJECT methods
      default_attr_value      FOR bi_object~default_attribute_value,
      execute_def_method      FOR bi_object~execute_default_method,
      release                 FOR bi_object~release,
*     BI_PERSISTENT methods
      find_by_lpor            FOR bi_persistent~find_by_lpor,
      lpor                    FOR bi_persistent~lpor,
      refresh                 FOR bi_persistent~refresh.

*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">My type Id</p>
      c_my_typeid             TYPE sibftypeid        VALUE 'ZCL_CA_OBJ_STATUS'  ##no_text.

*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">Language key for external status values</p>
      mv_langu          TYPE syst_langu READ-ONLY.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Constants for commit mode</p>
      mo_commit_modes    TYPE REF TO zcl_ca_c_commit_mode READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Constants for object status type</p>
      mo_status_types    TYPE REF TO zcl_ca_c_obj_status_type READ-ONLY,

*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Status object information</p>
      ms_status_info     TYPE jsto    READ-ONLY,

*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">Status order number</p>
      mv_status_order_no TYPE j_stonr READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Key attribute - Status object number</p>
      mv_key             TYPE j_objnr READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Default attribute with prepared object key</p>
      mv_def_attr        TYPE text80  READ-ONLY.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Class constructor</p>
      class_constructor,

      "! <p class="shorttext synchronized" lang="en">Activate writing change documents</p>
      "!
      "! @parameter iv_active | <p class="shorttext synchronized" lang="en">X = Writing change documents is active</p>
      activate_change_docs
        IMPORTING
          iv_active TYPE abap_bool DEFAULT abap_true,

      "! <p class="shorttext synchronized" lang="en">Change language for status handling (default is German)</p>
      "!
      "! @parameter iv_langu | <p class="shorttext synchronized" lang="en">Language for status handling</p>
      change_language
        IMPORTING
          iv_langu TYPE syst_langu,

      "! <p class="shorttext synchronized" lang="en">Create instance or get from buffer</p>
      "!
      "! @parameter is_lpor      | <p class="shorttext synchronized" lang="en">Workflow instance key</p>
      "! @parameter iv_key       | <p class="shorttext synchronized" lang="en">Status object number</p>
      "! @parameter ro_instance  | <p class="shorttext synchronized" lang="en">Created instance or found in buffer</p>
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      "! @raising   zcx_ca_dbacc | <p class="shorttext synchronized" lang="en">Common exception: Database access</p>
      get_instance
        IMPORTING
          is_lpor            TYPE sibflpor OPTIONAL
          iv_key             TYPE j_objnr OPTIONAL
            PREFERRED PARAMETER iv_key
        RETURNING
          VALUE(ro_instance) TYPE REF TO zcl_ca_obj_status
        RAISING
          zcx_ca_param
          zcx_ca_dbacc.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
      "!
      "! @parameter iv_key       | <p class="shorttext synchronized" lang="en">Status object number</p>
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      "! @raising   zcx_ca_dbacc | <p class="shorttext synchronized" lang="en">Common exception: Database access</p>
      constructor
        IMPORTING
          iv_key TYPE j_objnr
        RAISING
          zcx_ca_param
          zcx_ca_dbacc,

      "! <p class="shorttext synchronized" lang="en">Display all status changes</p>
      display_chg_docs_overall,

      "! <p class="shorttext synchronized" lang="en">Get active status, all, system only or user status only</p>
      "!
      "! @parameter iv_status_types   | <p class="shorttext synchronized" lang="en">A=All; S=System status only; U=User status only</p>
      "! @parameter rv_status         | <p class="shorttext synchronized" lang="en">Active status</p>
      "! @raising   zcx_ca_obj_status | <p class="shorttext synchronized" lang="en">Common exception: Object status handler error</p>
      get_active
        IMPORTING
          iv_status_types  TYPE char1 DEFAULT zcl_ca_c_obj_status_type=>all
        RETURNING
          VALUE(rv_status) TYPE j_stext
        RAISING
          zcx_ca_obj_status,

      "! <p class="shorttext synchronized" lang="en">Get short text to a status</p>
      "!
      "! @parameter iv_ext_status     | <p class="shorttext synchronized" lang="en">External status (= language dependent value)</p>
      "! @parameter rv_short_text | <p class="shorttext synchronized" lang="en">Short text of the status</p>
      get_short_text
        IMPORTING
          iv_ext_status        TYPE j_txt04
        RETURNING
          VALUE(rv_short_text) TYPE j_txt30,

      "! <p class="shorttext synchronized" lang="en">Assemble task short text</p>
      "!
      "! @parameter iv_task_desc | <p class="shorttext synchronized" lang="en">Complementing description/action/function</p>
      "! @parameter rv_task_desc | <p class="shorttext synchronized" lang="en">Completed task short description</p>
      get_task_descr
        IMPORTING
          iv_task_desc        TYPE text80
        RETURNING
          VALUE(rv_task_desc) TYPE witext,

      "! <p class="shorttext synchronized" lang="en">Is the passed status currently active?</p>
      "!
      "! @parameter iv_ext_status     | <p class="shorttext synchronized" lang="en">External status (= language dependent value)</p>
      "! @parameter rv_is_active      | <p class="shorttext synchronized" lang="en">X = Status is currently active</p>
      "! @raising   zcx_ca_obj_status | <p class="shorttext synchronized" lang="en">Common exception: Object status handler error</p>
      is_active
        IMPORTING
          iv_ext_status       TYPE j_txt04
        RETURNING
          VALUE(rv_is_active) TYPE abap_bool
        RAISING
          zcx_ca_obj_status,

      "! <p class="shorttext synchronized" lang="en">Is passed status usable as next status?</p>
      "!
      "! @parameter iv_ext_status     | <p class="shorttext synchronized" lang="en">External status (= language dependent value)</p>
      "! @parameter rv_is_possible    | <p class="shorttext synchronized" lang="en">X = Status can be set as next status</p>
      "! @raising   zcx_ca_obj_status | <p class="shorttext synchronized" lang="en">Common exception: Object status handler error</p>
      is_possible_as_next
        IMPORTING
          iv_ext_status         TYPE j_txt04
        RETURNING
          VALUE(rv_is_possible) TYPE abap_bool
        RAISING
          zcx_ca_obj_status,

      "! <p class="shorttext synchronized" lang="en">Is passed status removable?</p>
      "!
      "! @parameter iv_ext_status     | <p class="shorttext synchronized" lang="en">External status (= language dependent value)</p>
      "! @parameter rv_is_removable   | <p class="shorttext synchronized" lang="en">X = Status is removable</p>
      "! @raising   zcx_ca_obj_status | <p class="shorttext synchronized" lang="en">Common exception: Object status handler error</p>
      is_removable
        IMPORTING
          iv_ext_status          TYPE j_txt04
        RETURNING
          VALUE(rv_is_removable) TYPE abap_bool
        RAISING
          zcx_ca_obj_status,

      "! <p class="shorttext synchronized" lang="en">Instanzereignis auslösen</p>
      "!
      "! @parameter iv_event             | <p class="shorttext synchronized" lang="en">Event name</p>
      "! @parameter iv_do_commit         | <p class="shorttext synchronized" lang="en">X = Do commit here</p>
      "! @raising   zcx_ca_param         | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      raise_event
        IMPORTING
          iv_event     TYPE sibfevent
          iv_do_commit TYPE abap_bool DEFAULT abap_false
        RAISING
          zcx_ca_param,

      "! <p class="shorttext synchronized" lang="en">Remove (delete) passed status</p>
      "!
      "! @parameter iv_ext_status     | <p class="shorttext synchronized" lang="en">External status (= language dependent value)</p>
      "! @parameter iv_commit_mode    | <p class="shorttext synchronized" lang="en">Type of commit (0=by caller; 1=asynch; 2=synch/wait)</p>
      "! @raising   zcx_ca_obj_status | <p class="shorttext synchronized" lang="en">Common exception: Object status handler error</p>
      remove
        IMPORTING
          iv_ext_status  TYPE j_txt04
          iv_commit_mode TYPE zca_d_commit_mode DEFAULT zcl_ca_c_commit_mode=>by_caller
        RAISING
          zcx_ca_obj_status,

      "! <p class="shorttext synchronized" lang="en">Set (change) status to passed value</p>
      "!
      "! @parameter iv_ext_status     | <p class="shorttext synchronized" lang="en">External status (= language dependent value)</p>
      "! @parameter iv_commit_mode    | <p class="shorttext synchronized" lang="en">Type of commit (0=by caller; 1=asynch; 2=synch/wait)</p>
      "! @raising   zcx_ca_obj_status | <p class="shorttext synchronized" lang="en">Common exception: Object status handler error</p>
      set_new
        IMPORTING
          iv_ext_status  TYPE j_txt04
          iv_commit_mode TYPE zca_d_commit_mode DEFAULT zcl_ca_c_commit_mode=>by_caller
        RAISING
          zcx_ca_obj_status.

*   i n s t a n c e   e v e n t s
    EVENTS:
      "! <p class="shorttext synchronized" lang="en">Object changed</p>
      changed.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   a l i a s e s
    ALIASES:
*     Message types
      c_msgty_e            FOR  if_xo_const_message~error,
      c_msgty_i            FOR  if_xo_const_message~info,
      c_msgty_s            FOR  if_xo_const_message~success,
      c_msgty_w            FOR  if_xo_const_message~warning.
**     Event names
*      c_evt_name_created   FOR zif_ca_c_wf_evt_names~c_evt_name_created,
*      c_evt_name_changed   FOR zif_ca_c_wf_evt_names~c_evt_name_changed.

*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     s i n g l e   v a l u e s
*      "! <p class="shorttext synchronized" lang="en">Language key for external status values</p>
*      mv_langu          TYPE syst_langu,
      "! <p class="shorttext synchronized" lang="en">X = Writing change documents is active</p>
      mv_chgdocs_active TYPE abap_bool.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Constants for object internal status type</p>
      mo_int_status_types TYPE REF TO zcl_ca_c_obj_int_status_type,

*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">Current status</p>
      mt_status           TYPE ttjstat,

*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Workflow object instance key</p>
      ms_lpor             TYPE sibflpor.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Check existence of object</p>
      "!
      "! @parameter iv_key       | <p class="shorttext synchronized" lang="en">Status object number</p>
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      "! @raising   zcx_ca_dbacc | <p class="shorttext synchronized" lang="en">Common exception: Database access</p>
      check_existence
        IMPORTING
          iv_key TYPE j_objnr
        RAISING
          zcx_ca_param
          zcx_ca_dbacc,

      "! <p class="shorttext synchronized" lang="en">Get current active status</p>
      "!
      "! @parameter iv_objnr          | <p class="shorttext synchronized" lang="en">Status object number</p>
      "! @raising   zcx_ca_obj_status | <p class="shorttext synchronized" lang="en">Common exception: Object status handler error</p>
      get_current
        IMPORTING
          iv_objnr TYPE j_objnr
        RAISING
          zcx_ca_obj_status,

      "! <p class="shorttext synchronized" lang="en">Convert external status into internal</p>
      "!
      "! @parameter iv_ext_status     | <p class="shorttext synchronized" lang="en">External status (= language dependent value)</p>
      "! @parameter iv_status_type    | <p class="shorttext synchronized" lang="en">'I' = System status / 'E' = User status</p>
      "! @parameter rv_int_status     | <p class="shorttext synchronized" lang="en">Internal status</p>
      "! @raising   zcx_ca_obj_status | <p class="shorttext synchronized" lang="en">Common exception: Object status handler error</p>
      convert_ext_2_int
        IMPORTING
          iv_ext_status        TYPE j_txt04
          iv_status_type       TYPE char1 DEFAULT zcl_ca_c_obj_int_status_type=>user
        RETURNING
          VALUE(rv_int_status) TYPE j_status
        RAISING
          zcx_ca_obj_status,

      "! <p class="shorttext synchronized" lang="en">Change or check an external status</p>
      "!
      "! @parameter iv_ext_status     | <p class="shorttext synchronized" lang="en">External status (= language dependent value)</p>
      "! @parameter iv_check_only     | <p class="shorttext synchronized" lang="en">X = Execute only a check if the operation is possible</p>
      "! @parameter iv_remove         | <p class="shorttext synchronized" lang="en">X = Remove the status</p>
      "! @parameter iv_check_auth     | <p class="shorttext synchronized" lang="en">X = Do an authority check for this operation</p>
      "! @parameter iv_commit_mode    | <p class="shorttext synchronized" lang="en">Type of commit (0=by caller; 1=asynch; 2=synch/wait)</p>
      "! @parameter rv_is_possible    | <p class="shorttext synchronized" lang="en">X = Operation is possible (only when checking)</p>
      "! @raising   zcx_ca_obj_status | <p class="shorttext synchronized" lang="en">Common exception: Object status handler error</p>
      change
        IMPORTING
          iv_ext_status         TYPE j_txt04
          iv_check_only         TYPE abap_bool DEFAULT abap_false
          iv_remove             TYPE abap_bool DEFAULT abap_false
          iv_check_auth         TYPE abap_bool DEFAULT abap_true
          iv_commit_mode        TYPE zca_d_commit_mode DEFAULT zcl_ca_c_commit_mode=>by_caller
        RETURNING
          VALUE(rv_is_possible) TYPE abap_bool
        RAISING
          zcx_ca_obj_status,

      "! <p class="shorttext synchronized" lang="en">Is a change of the requested status allowed?</p>
      "!
      "! @parameter iv_ext_status     | <p class="shorttext synchronized" lang="en">External status (= language dependent value)</p>
      "! @parameter iv_check_only     | <p class="shorttext synchronized" lang="en">X = Execute only a check if the operation is possible</p>
      "! @parameter iv_int_status     | <p class="shorttext synchronized" lang="en">Internal status</p>
      "! @raising   zcx_ca_obj_status | <p class="shorttext synchronized" lang="en">Common exception: Object status handler error</p>
      is_change_of_status_allowed
        IMPORTING
          iv_ext_status TYPE j_txt04
          iv_check_only TYPE abap_bool
          iv_int_status TYPE j_status
        RAISING
          zcx_ca_obj_status,

      "! <p class="shorttext synchronized" lang="en">Check result after changing status</p>
      "!
      "! @parameter iv_check_only     | <p class="shorttext synchronized" lang="en">X = Execute only a check if the operation is possible</p>
      "! @parameter iv_func_module    | <p class="shorttext synchronized" lang="en">Function module name for exception</p>
      "! @parameter iv_subrc          | <p class="shorttext synchronized" lang="en">Return code of function module for exception</p>
      "! @parameter rv_is_possible    | <p class="shorttext synchronized" lang="en">X = Operation is possible (only when checking)</p>
      "! @raising   zcx_ca_obj_status | <p class="shorttext synchronized" lang="en">Common exception: Object status handler error</p>
      check_return_result
        IMPORTING
          iv_check_only         TYPE abap_bool
          iv_func_module        TYPE rs38l_fnam
          VALUE(iv_subrc)       TYPE syst_subrc
        RETURNING
          VALUE(rv_is_possible) TYPE abap_bool
        RAISING
          zcx_ca_obj_status.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   l o c a l   t y p e   d e f i n i t i o n
    TYPES:
      "! <p class="shorttext synchronized" lang="en">Buffered instance</p>
      BEGIN OF ty_s_buffer.
        INCLUDE TYPE sibflpor AS s_lpor.
    TYPES:
        o_instance TYPE REF TO zcl_ca_obj_status,
      END   OF ty_s_buffer,
      "! <p class="shorttext synchronized" lang="en">Instance buffer</p>
      ty_t_buffer TYPE SORTED TABLE OF ty_s_buffer
                                       WITH UNIQUE KEY s_lpor.

*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">Instance buffer</p>
      mt_buffer     TYPE ty_t_buffer.

ENDCLASS.



CLASS ZCL_CA_OBJ_STATUS IMPLEMENTATION.


  METHOD class_constructor.
    "-----------------------------------------------------------------*
    "   Class constructor
    "-----------------------------------------------------------------*
    "Clear buffer with first execution
    CALL FUNCTION 'STATUS_BUFFER_REFRESH'
      EXPORTING
        i_free = abap_true.

    "Set defaults
    mv_langu          = sy-langu.
    mv_chgdocs_active = abap_false.     "Writing change documents is inactive
  ENDMETHOD.                    "class_constructor


  METHOD change_language.
    "-----------------------------------------------------------------*
    "   Change language for status handling (default is German)
    "-----------------------------------------------------------------*
    mv_langu = iv_langu.
  ENDMETHOD.                    "change_language


  METHOD get_instance.
    "-----------------------------------------------------------------*
    "   Get instance
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      ls_lpor TYPE sibflpor,
      lv_key  TYPE j_objnr.

    IF is_lpor IS NOT INITIAL.
      ls_lpor       = is_lpor.
      ls_lpor-catid = swfco_objtype_cl.

      "Set key into structured definition
      IF ls_lpor-instid IS NOT INITIAL.  "Avoid destruction of type conform initial values
        lv_key = CONV #( ls_lpor-instid ).
      ENDIF.

      "Set these values in any case, e. g. to create/get an instance only with the key string
      IF ls_lpor-typeid IS INITIAL.
        ls_lpor-typeid = zcl_ca_obj_status=>c_my_typeid.
      ENDIF.

    ELSEIF iv_key IS NOT INITIAL.
      lv_key = iv_key.
      ls_lpor = VALUE #( instid = CONV #( iv_key )
                         typeid = zcl_ca_obj_status=>c_my_typeid
                         catid  = swfco_objtype_cl ).

    ELSE.
      "At least one of the following parameters must be passed: &1 &2 &3 &4
      RAISE EXCEPTION TYPE zcx_ca_param
        EXPORTING
          textid   = zcx_ca_param=>at_least_one
          mv_msgty = c_msgty_e
          mv_msgv1 = 'IS_LPOR'
          mv_msgv2 = 'IV_KEY'
          mv_msgv3 = space
          mv_msgv4 = space ##no_text.
    ENDIF.

    "If the key is still not available create no instance
    IF lv_key IS INITIAL.
      RETURN.
    ENDIF.

    "Is an instance already created?
    READ TABLE mt_buffer INTO DATA(ls_buffer)
                         WITH KEY s_lpor = ls_lpor.
    IF sy-subrc EQ 0.
      "Refresh some data
      ls_buffer-o_instance->refresh( ).

    ELSE.
      "Create instance of payment approval object
      CREATE OBJECT ls_buffer-o_instance TYPE (ls_lpor-typeid)
        EXPORTING
          iv_key = lv_key.

      ls_buffer-s_lpor = ls_buffer-o_instance->lpor( ).
      INSERT ls_buffer INTO TABLE mt_buffer.
    ENDIF.

    ro_instance = ls_buffer-o_instance.
  ENDMETHOD.                    "get_instance


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    ms_lpor-typeid = c_my_typeid.
    ms_lpor-catid  = swfco_objtype_cl.

    IF iv_key IS INITIAL.
      RETURN.
    ENDIF.

    "Check existence of object
    check_existence( iv_key ).

    "Complete and keep several attributes
    mv_key         = iv_key.
    ms_lpor-instid = mv_key.

    "Set default attribute = resolved into readable key
    default_attr_value( ).

    mo_commit_modes     = zcl_ca_c_commit_mode=>get_instance( ).
    mo_status_types     = zcl_ca_c_obj_status_type=>get_instance( ).
    mo_int_status_types = zcl_ca_c_obj_int_status_type=>get_instance( ).
  ENDMETHOD.                    "constructor


  METHOD check_existence.
    "-----------------------------------------------------------------*
    "   Check existence of object
    "-----------------------------------------------------------------*
    CALL FUNCTION 'STATUS_OBJECT_READ'
      EXPORTING
        objnr            = iv_key
      IMPORTING
        e_jsto           = ms_status_info
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.
    CASE sy-subrc.
      WHEN 0.
        "Aktuelle Status einlesen
        get_current( iv_key ).

      WHEN 1.
        DATA(lx_dbacc) =
             CAST zcx_ca_dbacc(
                    zcx_ca_error=>create_exception(
                             iv_excp_cls = zcx_ca_dbacc=>c_zcx_ca_dbacc
                             iv_function = 'STATUS_OBJECT_READ'
                             iv_subrc    = sy-subrc  ) )  ##no_text.
        IF lx_dbacc IS BOUND.
          RAISE EXCEPTION lx_dbacc.
        ENDIF.

      WHEN OTHERS.
        DATA(lx_error) =
             CAST zcx_ca_obj_status(
                    zcx_ca_error=>create_exception(
                             iv_excp_cls = zcx_ca_obj_status=>c_zcx_ca_obj_status
                             iv_function = 'STATUS_OBJECT_READ'
                             iv_subrc    = sy-subrc ) )  ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDCASE.
  ENDMETHOD.                    "check_existence


  METHOD set_new.
    "-----------------------------------------------------------------*
    "   Set (change) status to passed value
    "-----------------------------------------------------------------*
    change( iv_ext_status  = iv_ext_status
            iv_commit_mode = iv_commit_mode ).
  ENDMETHOD.                    "set_new


  METHOD remove.
    "-----------------------------------------------------------------*
    "   Remove (delete) passed status
    "-----------------------------------------------------------------*
    change( iv_ext_status  = iv_ext_status
            iv_remove      = abap_true
            iv_commit_mode = iv_commit_mode ).
  ENDMETHOD.                    "remove


  METHOD raise_event.
    "-----------------------------------------------------------------*
    "   Raise event
    "-----------------------------------------------------------------*
*    zcl_ca_wf_wapi_utils=>create_event_extended(
*                                  is_lpor      = CORRESPONDING #( ms_lpor )
*                                  iv_event     = iv_event
*                                  iv_do_commit = iv_do_commit ).
  ENDMETHOD.                    "raise_event


  METHOD is_removable.
    "-----------------------------------------------------------------*
    "   Is passed status removable?
    "-----------------------------------------------------------------*
    rv_is_removable = change( iv_ext_status = iv_ext_status
                              iv_check_only = abap_true
                              iv_remove     = abap_true ).
  ENDMETHOD.                    "is_removable


  METHOD is_possible_as_next.
    "-----------------------------------------------------------------*
    "   Is passed status usable as next status?
    "-----------------------------------------------------------------*
    rv_is_possible = change( iv_ext_status = iv_ext_status
                             iv_check_only = abap_true
                             iv_check_auth = abap_false ).
  ENDMETHOD.                    "is_possible_as_next


  METHOD is_active.
    "-----------------------------------------------------------------*
    "   Is the passed status currently active?
    "-----------------------------------------------------------------*
    CALL FUNCTION 'STATUS_CHECK'
      EXPORTING
        objnr             = mv_key
        status            = convert_ext_2_int( iv_ext_status )
      EXCEPTIONS
        status_not_active = 1
        object_not_found  = 2
        OTHERS            = 3.
    CASE sy-subrc.
      WHEN 0.
        rv_is_active = abap_true.

      WHEN 1.
        rv_is_active = abap_false.

      WHEN OTHERS.
        DATA(lx_error) =
             CAST zcx_ca_obj_status(
                    zcx_ca_error=>create_exception(
                             iv_excp_cls = zcx_ca_obj_status=>c_zcx_ca_obj_status
                             iv_function = 'STATUS_CHECK'
                             iv_subrc    = sy-subrc ) )  ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDCASE.
  ENDMETHOD.                    "is_active


  METHOD get_task_descr.
    "-----------------------------------------------------------------*
    "   Assemble task short text
    "-----------------------------------------------------------------*
    rv_task_desc = |{ mv_def_attr } - { iv_task_desc }|.

    "Use this statement in your task, here in this sample for a background step
*    &_WI_OBJECT_ID.GET_TASK_DESCR(IV_TASK_DESC='Beleg buchen (BG)')&
  ENDMETHOD.                    "get_task_descr


  METHOD get_short_text.
    "-----------------------------------------------------------------*
    "   Get active user status
    "-----------------------------------------------------------------*
    TRY.
        DATA(lv_int_status) = convert_ext_2_int( iv_ext_status ).

        SELECT SINGLE txt30 INTO  rv_short_text
                            FROM  tj30t
                            WHERE stsma EQ ms_status_info-stsma
                              AND estat EQ lv_int_status
                              AND spras EQ mv_langu.
        IF sy-subrc NE 0.
          SELECT SINGLE txt30 INTO  rv_short_text
                              FROM  tj02t
                              WHERE istat EQ lv_int_status
                                AND spras EQ mv_langu.
        ENDIF.

      CATCH zcx_ca_obj_status INTO DATA(lx_catched).
        rv_short_text = lx_catched->get_text( ).
    ENDTRY.
  ENDMETHOD.                    "get_short_text


  METHOD get_current.
    "-----------------------------------------------------------------*
    "   Get current active status
    "-----------------------------------------------------------------*
    CALL FUNCTION 'STATUS_READ'
      EXPORTING
        objnr            = iv_objnr    " Objektnummer
        only_active      = abap_true
      IMPORTING
*       obtyp            = ms_status_info-obtyp
*       stsma            = ms_status_info-stsma
        stonr            = mv_status_order_no
      TABLES
        status           = mt_status
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.
    IF sy-subrc NE 0.
      DATA(lx_error) =
           CAST zcx_ca_obj_status(
                  zcx_ca_error=>create_exception(
                           iv_excp_cls = zcx_ca_obj_status=>c_zcx_ca_obj_status
                           iv_function = 'STATUS_OBJECT_READ'
                           iv_subrc    = sy-subrc ) )  ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "get_current


  METHOD get_active.
    "-----------------------------------------------------------------*
    "   Get active system and user status
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lv_system_status TYPE j_stext,
      lv_user_status   TYPE j_stext.

    CALL FUNCTION 'STATUS_TEXT_EDIT'
      EXPORTING
        flg_user_stat    = abap_true    "Flag 'Edit User Status'
        objnr            = mv_key
        spras            = mv_langu
        only_active      = abap_true
      IMPORTING
        line             = lv_system_status
        user_line        = lv_user_status
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.
    IF sy-subrc NE 0.
      DATA(lx_error) =
           CAST zcx_ca_obj_status(
                  zcx_ca_error=>create_exception(
                           iv_excp_cls = zcx_ca_obj_status=>c_zcx_ca_obj_status
                           iv_function = 'STATUS_TEXT_EDIT'
                           iv_subrc    = sy-subrc ) )  ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.

    CASE iv_status_types.
      WHEN mo_status_types->all.
        rv_status = condense( |{ lv_system_status } { lv_user_status }| ).

      WHEN mo_status_types->sys_only.
        rv_status = lv_system_status.

      WHEN mo_status_types->user_only.
        rv_status = lv_user_status.
    ENDCASE.
  ENDMETHOD.                    "get_active


  METHOD display_chg_docs_overall.
    "-----------------------------------------------------------------*
    "   Display all status changes
    "-----------------------------------------------------------------*
    CALL FUNCTION 'STATUS_SHOW_CHANGE_DOCUMENTS'
      EXPORTING
        objnr                 = mv_key
        show_all              = abap_true
      EXCEPTIONS
        no_chg_doc_for_object = 1
        OTHERS                = 2.
    CASE sy-subrc.
      WHEN 0.
        "Everything is ok

      WHEN 1.
        MESSAGE ID sy-msgid TYPE c_msgty_i NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

      WHEN OTHERS.
        MESSAGE ID sy-msgid TYPE c_msgty_e NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDCASE.
  ENDMETHOD.                    "display_chg_docs_overall


  METHOD convert_ext_2_int.
    "-----------------------------------------------------------------*
    "   Convert external status into internal
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lx_error       TYPE REF TO zcx_ca_obj_status,
      lv_langu_found TYPE syst_langu.

    CALL FUNCTION 'STATUS_TEXT_CONVERSION'
      EXPORTING
        language           = mv_langu
        mode               = iv_status_type
        objnr              = mv_key
        stsma              = ms_status_info-stsma
        txt04              = iv_ext_status
      IMPORTING
        language_found     = lv_langu_found
        status_number      = rv_int_status
      EXCEPTIONS
        not_found          = 1
        insufficient_input = 2
        object_not_found   = 3
        wrong_mode         = 4
        OTHERS             = 5.
    CASE sy-subrc.
      WHEN 0.
        IF lv_langu_found NE mv_langu.
          "External status &1 is not supported in language &2
          WRITE mv_langu USING EDIT MASK '==ISOLA' LEFT-JUSTIFIED TO sy-msgv2.
          RAISE EXCEPTION TYPE zcx_ca_obj_status
            EXPORTING
              textid   = zcx_ca_obj_status=>language_not_supported
              mv_msgty = c_msgty_e
              mv_msgv1 = CONV #( iv_ext_status )
              mv_msgv2 = sy-msgv2.
        ENDIF.

      WHEN 1.
        CASE iv_status_type.
          WHEN mo_int_status_types->user.
            "Retry for a system status
            rv_int_status = convert_ext_2_int( iv_ext_status  = iv_ext_status
                                               iv_status_type = mo_int_status_types->system ).

          WHEN mo_int_status_types->system.
            lx_error ?= zcx_ca_error=>create_exception(
                                 iv_excp_cls = zcx_ca_obj_status=>c_zcx_ca_obj_status
                                 iv_function = 'STATUS_TEXT_CONVERSION'
                                 iv_subrc    = sy-subrc )  ##no_text.
            IF lx_error IS BOUND.
              RAISE EXCEPTION lx_error.
            ENDIF.
        ENDCASE.

      WHEN OTHERS.
        lx_error ?= zcx_ca_error=>create_exception(
                             iv_excp_cls = zcx_ca_obj_status=>c_zcx_ca_obj_status
                             iv_function = 'STATUS_TEXT_CONVERSION'
                             iv_subrc    = sy-subrc )  ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDCASE.
  ENDMETHOD.                    "convert_ext_2_int


  METHOD change.
    "-----------------------------------------------------------------*
    "   Change or check an external status
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
*      lx_error       TYPE REF TO zcx_ca_obj_status,
      lt_int_status  TYPE ttjstat,
      lv_func_module TYPE rs38l_fnam.

    rv_is_possible = abap_true.

    DATA(lv_int_status) = convert_ext_2_int( iv_ext_status ).

    is_change_of_status_allowed( iv_ext_status = iv_ext_status
                                 iv_check_only = iv_check_only
                                 iv_int_status = lv_int_status ).

    mo_commit_modes->is_valid( iv_commit_mode ).

    CASE lv_int_status(1).
      WHEN mo_int_status_types->system.
        "S y s t e m   s t a t u s
        lt_int_status = VALUE #( ( stat  = lv_int_status
                                   inact = iv_remove ) ).

        lv_func_module = 'STATUS_CHANGE_INTERN' ##no_text.    "set for exception handling
        CALL FUNCTION 'STATUS_CHANGE_INTERN'
          EXPORTING
            objnr               = mv_key
            check_only          = iv_check_only
            set_chgkz           = mv_chgdocs_active
          TABLES
            status              = lt_int_status
          EXCEPTIONS
            status_not_allowed  = 1
            object_not_found    = 2
            status_inconsistent = 3
            OTHERS              = 4.

      WHEN mo_int_status_types->user.
        "U s e r   s t a t u s
        lv_func_module = 'STATUS_CHANGE_EXTERN' ##no_text.    "set for exception handling
        CALL FUNCTION 'STATUS_CHANGE_EXTERN'
          EXPORTING
            objnr               = mv_key
            user_status         = lv_int_status
            check_only          = iv_check_only
            set_inact           = iv_remove
            no_check            = iv_check_auth
            set_chgkz           = mv_chgdocs_active
          EXCEPTIONS
            status_not_allowed  = 1
            object_not_found    = 2
            status_inconsistent = 3
            OTHERS              = 4.
    ENDCASE.

    rv_is_possible = check_return_result( iv_check_only  = iv_check_only
                                          iv_subrc       = sy-subrc
                                          iv_func_module = lv_func_module ).

    IF iv_check_only EQ abap_false.
      zcl_ca_utils=>do_commit( iv_commit_mode = iv_commit_mode ).
    ENDIF.
  ENDMETHOD.                    "change


  METHOD is_change_of_status_allowed.
    "-----------------------------------------------------------------*
    "   Is a change of the requested status allowed?
    "-----------------------------------------------------------------*
    IF iv_int_status(1) EQ mo_int_status_types->system AND
       iv_check_only    EQ abap_false.
      "Changing a system status (&1) with this method is not allowed
      RAISE EXCEPTION TYPE zcx_ca_obj_status
        EXPORTING
          textid   = zcx_ca_obj_status=>chg_syst_stat_not_allowed
          mv_msgty = c_msgty_e
          mv_msgv1 = CONV #( iv_ext_status ).
    ENDIF.

  ENDMETHOD.                    "is_change_of_status_allowed


  METHOD check_return_result.
    "-----------------------------------------------------------------*
    "   Check result after changing status
    "-----------------------------------------------------------------*
    IF iv_subrc NE 0.
      IF iv_check_only EQ abap_true AND
         iv_subrc      EQ 1.
        rv_is_possible = abap_false.

      ELSE.
        DATA(lx_error) =
               CAST zcx_ca_obj_status(
                         zcx_ca_error=>create_exception(
                                     iv_excp_cls = zcx_ca_obj_status=>c_zcx_ca_obj_status
                                     iv_function = iv_func_module
                                     iv_subrc    = iv_subrc ) ) ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "check_return_result


  METHOD bi_persistent~refresh ##needed.
    "-----------------------------------------------------------------*
    "   Refresh instance
    "-----------------------------------------------------------------*
    TRY.
        get_current( mv_key ).

      CATCH zcx_ca_error INTO DATA(lx_catched).
        "As long as no exceptions are declared for this method, this is
        "currently the best solution.
        MESSAGE lx_catched TYPE c_msgty_s DISPLAY LIKE lx_catched->mv_msgty.
    ENDTRY.
  ENDMETHOD.                    "bi_persistent~refresh


  METHOD bi_persistent~lpor.
    "-----------------------------------------------------------------*
    "   Return instance key
    "-----------------------------------------------------------------*
    result = ms_lpor.
  ENDMETHOD.                    "bi_persistent~lpor


  METHOD bi_persistent~find_by_lpor.
    "-----------------------------------------------------------------*
    "   Create workflow / workitem instance
    "-----------------------------------------------------------------*
    TRY.
        result = CAST bi_persistent(
                         zcl_ca_obj_status=>get_instance( is_lpor = lpor ) ).

      CATCH zcx_ca_error INTO DATA(lx_catched).
        "As long as no exceptions are declared for this method, this is
        "currently the best solution.
        MESSAGE lx_catched TYPE c_msgty_s DISPLAY LIKE lx_catched->mv_msgty.
    ENDTRY.
  ENDMETHOD.                    "bi_persistent~find_by_lpor


  METHOD bi_object~release ##needed.
    "-----------------------------------------------------------------*
    "   Release instance
    "-----------------------------------------------------------------*
    DELETE mt_buffer WHERE s_lpor EQ ms_lpor.
  ENDMETHOD.                    "bi_object~release


  METHOD bi_object~execute_default_method.
    "-----------------------------------------------------------------*
    "   Execute default method
    "-----------------------------------------------------------------*
    display_chg_docs_overall( ).
  ENDMETHOD.                    "bi_object~execute_default_method


  METHOD bi_object~default_attribute_value.
    "-----------------------------------------------------------------*
    "   Returns a description and/or prepared key of the object.
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lv_obj_ident TYPE j_objnr_out,
      lv_obj_descr TYPE j_text60.

    "TEXT-OST = Status object to aaa kkkkkkkk tttttttttttttt
    CALL FUNCTION 'OBJECT_IDENTIFICATION_GET'
      EXPORTING
        objnr            = mv_key
      IMPORTING
        e_text           = lv_obj_descr
        e_identification = lv_obj_ident
      EXCEPTIONS
        obart_invalid    = 1
        OTHERS           = 2.
    IF sy-subrc NE 0.
      MESSAGE ID   sy-msgid  TYPE sy-msgty  NUMBER sy-msgno
              WITH sy-msgv1  sy-msgv2  sy-msgv3  sy-msgv4  INTO lv_obj_descr.
    ENDIF.

    mv_def_attr = condense( |{ TEXT-ost } { lv_obj_ident } { lv_obj_descr }| ).
    result = REF #( mv_def_attr ).
  ENDMETHOD.                    "bi_object~default_attribute_value


  METHOD activate_change_docs.
    "-----------------------------------------------------------------*
    "   Activate writing change documents
    "-----------------------------------------------------------------*
    mv_chgdocs_active = iv_active.
  ENDMETHOD.                    "activate_change_docs
ENDCLASS.
