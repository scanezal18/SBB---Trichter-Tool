INTERFACE zif_rs_3dpc_abrufstatus
  PUBLIC .
  TYPES ts_vorhaben TYPE zrs_3tt_vorhaben_s.
  TYPES ts_abrufkopf TYPE zrs_3tt_abrufkopf_s .
  TYPES ts_abrufpos TYPE zrs_3tt_abrufpos_s .
  TYPES ts_verrchng TYPE zrs_3tt_verrchng_s.
  TYPES:
    tt_vorhaben TYPE STANDARD TABLE OF ts_vorhaben WITH DEFAULT KEY .
  TYPES:
    tt_abrufkopf TYPE STANDARD TABLE OF ts_abrufkopf WITH DEFAULT KEY .
  TYPES:
    tt_abrufpos  TYPE STANDARD TABLE OF ts_abrufpos WITH DEFAULT KEY .
  TYPES: tt_verrchng TYPE STANDARD TABLE OF ts_verrchng WITH DEFAULT KEY.
  TYPES: ts_mo_context TYPE REF TO /IWBEP/IF_MGW_CONTEXT.

  TYPES: BEGIN OF ts_email_params,
           iv_mail_id             TYPE zse_mail_id,
           iv_project_id          TYPE zrs_3tt_vorhaben-vorhabensnummer,
           iv_sender_address      TYPE string,
           iv_sender_address_type TYPE so_escape,
           iv_language            TYPE spras,
           iv_send_immediately    TYPE flag,
           iv_html                TYPE flag,
           iv_subject_as_title    TYPE flag,
           it_name_value          TYPE zrs_3tt_name_value_t,
           it_recipients          TYPE zrs_3tt_mail_rec_t,
           it_text_table          TYPE soli_tab,
           ev_error               TYPE flag,
           ct_messages            TYPE bapiret2_t,
         END OF ts_email_params.

  TYPES:
    BEGIN OF ts_input_parameters,
      iv_entity_name     TYPE string,
      iv_entity_set_name TYPE string,
      iv_source_name     TYPE string,
      is_cud_type        TYPE string,
      er_entity_vorhaben TYPE ts_vorhaben,
      er_entity_kopf     TYPE ts_abrufkopf,
      er_entity_pos      TYPE ts_abrufpos,
      er_entity_kopf_old TYPE ts_abrufkopf,
      er_entity_pos_old  TYPE ts_abrufpos,
      er_entity_billing  TYPE ts_verrchng,
      es_email_params    TYPE ts_email_params,
      es_mo_context      TYPE ts_mo_context,
    END OF ts_input_parameters .

  METHODS check
    RETURNING
      VALUE(rv_if_object) TYPE REF TO zif_rs_3dpc_abrufstatus
    RAISING
      /iwbep/cx_mgw_tech_exception
      /iwbep/cx_mgw_busi_exception .
  METHODS get_results
    EXPORTING
      !es_entity_vorhaben TYPE ts_vorhaben
      !es_entity_kopf     TYPE ts_abrufkopf
      !es_entity_pos      TYPE ts_abrufpos
      !es_entity_kopf_old TYPE ts_abrufkopf
      !es_entity_pos_old  TYPE ts_abrufpos
      !es_entity_billing  TYPE ts_verrchng
      !es_email_params  TYPE ts_email_params
      !es_mo_context    TYPE ts_mo_context
    RAISING
      /iwbep/cx_mgw_tech_exception
      /iwbep/cx_mgw_busi_exception .
ENDINTERFACE.
