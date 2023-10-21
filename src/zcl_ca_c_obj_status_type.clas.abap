"! <p class="shorttext synchronized" lang="en">CA-TBX: Constants for object status type</p>
CLASS zcl_ca_c_obj_status_type DEFINITION PUBLIC
                                          CREATE PROTECTED.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Object status type: Get status of all types</p>
      all       TYPE char1             VALUE 'A' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Object status type: Get only system status</p>
      sys_only  TYPE char1             VALUE 'S' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Object status type: Get only user status</p>
      user_only TYPE char1             VALUE 'U' ##no_text.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Get instance</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">Class instance</p>
      get_instance
        RETURNING
          VALUE(result) TYPE REF TO zcl_ca_c_obj_status_type.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Is it a valid object status type?</p>
      "!
      "! @parameter status_type | <p class="shorttext synchronized" lang="en">Status type</p>
      is_object_status_valid FINAL
        IMPORTING
          status_type TYPE char1.


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
      singleton_instance     TYPE REF TO zcl_ca_c_obj_status_type.

ENDCLASS.



CLASS ZCL_CA_C_OBJ_STATUS_TYPE IMPLEMENTATION.


  METHOD is_object_status_valid.
    "-----------------------------------------------------------------*
    "   Is it a valid object status?
    "-----------------------------------------------------------------*
    "Check values
    IF status_type CN 'ASU' ##no_text.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_intern
        EXPORTING
          textid   = zcx_ca_intern=>param_invalid
          mv_msgty = 'E'
          mv_msgv1 = 'IV_STATUS_TYPE' ##no_text
          mv_msgv2 = CONV #( status_type ).
    ENDIF.
  ENDMETHOD.                    "is_object_status_valid


  METHOD get_instance.
    "-----------------------------------------------------------------*
    "   Get instance
    "-----------------------------------------------------------------*
    IF zcl_ca_c_obj_status_type=>singleton_instance IS NOT BOUND.
      zcl_ca_c_obj_status_type=>singleton_instance = NEW #( ).
    ENDIF.

    result = zcl_ca_c_obj_status_type=>singleton_instance.
  ENDMETHOD.                    "get_instance


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*

  ENDMETHOD.                    "constructor
ENDCLASS.
