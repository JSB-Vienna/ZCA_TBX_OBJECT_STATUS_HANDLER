"! <p class="shorttext synchronized" lang="en">CA-TBX: Constants for object internal status type</p>
CLASS zcl_ca_c_obj_int_status_type DEFINITION PUBLIC
                                              CREATE PROTECTED.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Internal object status type: System</p>
      system TYPE char1             VALUE 'I'  ##no_text,
      "! <p class="shorttext synchronized" lang="en">Internal object status type: User</p>
      user   TYPE char1             VALUE 'E'  ##no_text.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Get instance</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">Class instance</p>
      get_instance
        RETURNING
          VALUE(result) TYPE REF TO zcl_ca_c_obj_int_status_type.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Is the internal status type valid?</p>
      "!
      "! @parameter int_status_type | <p class="shorttext synchronized" lang="en">Internal status type</p>
      is_internal_status_type_valid FINAL
        IMPORTING
          int_status_type TYPE char1.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      constructor.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Instance of the class itself</p>
      singleton_instance     TYPE REF TO zcl_ca_c_obj_int_status_type.

ENDCLASS.



CLASS ZCL_CA_C_OBJ_INT_STATUS_TYPE IMPLEMENTATION.


  METHOD is_internal_status_type_valid.
    "-----------------------------------------------------------------*
    "   Is it a valid internal status type?
    "-----------------------------------------------------------------*
    "Check values
    IF int_status_type CN 'IE' ##no_text.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_intern
        EXPORTING
          textid   = zcx_ca_intern=>param_invalid
          mv_msgty = 'E'
          mv_msgv1 = 'IV_INT_STATUS_TYPE' ##no_text
          mv_msgv2 = CONV #( int_status_type ).
    ENDIF.
  ENDMETHOD.                    "is_internal_status_type_valid


  METHOD get_instance.
    "-----------------------------------------------------------------*
    "   Get instance
    "-----------------------------------------------------------------*
    IF zcl_ca_c_obj_int_status_type=>singleton_instance IS NOT BOUND.
      zcl_ca_c_obj_int_status_type=>singleton_instance = NEW #( ).
    ENDIF.

    result = zcl_ca_c_obj_int_status_type=>singleton_instance.
  ENDMETHOD.                    "get_instance


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*

  ENDMETHOD.                    "constructor
ENDCLASS.
