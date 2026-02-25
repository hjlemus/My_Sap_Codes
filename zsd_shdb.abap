*&---------------------------------------------------------------------*
*& Report  ZSDSHDB
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zsdshdb LINE-SIZE 250.

TABLES: dd03m, ctu_params.
DATA ti_bdcdata LIKE TABLE OF bdcdata WITH HEADER LINE.
DATA ti_shdb LIKE TABLE OF bdcdata WITH HEADER LINE.
DATA ca_tcode TYPE tcode.
DATA: v_text(180).
DATA: tmp_path     LIKE rlgrap-filename,
      tmp_filename LIKE rlgrap-filename,
      tmp_mask(80),
      pa_path11    TYPE string.


DATA: ind      TYPE i, actw(1), solo1vez VALUE 'N',
      s1       VALUE 'N', s2 VALUE 'N', s3 VALUE 'N', s4 VALUE 'N'.

DATA: BEGIN OF valsep OCCURS 0,        "Campo TXT Separados
        campos(132),
      END   OF valsep.

DATA: BEGIN OF datos OCCURS 0,         "Valores del TXT
        valores(4096),
      END OF datos.
* 2048
DATA: BEGIN OF estruc OCCURS 0,         "Estructura para excel
        valor(4096),
      END OF estruc.

* Identificador
SELECTION-SCREEN BEGIN OF BLOCK bl000 WITH FRAME TITLE text-t00.
PARAMETERS pa_shdb  TYPE apq_grpn MATCHCODE OBJECT zapqi.
SELECTION-SCREEN END OF BLOCK bl000.

* Archivo origen
SELECTION-SCREEN BEGIN OF BLOCK bl001 WITH FRAME TITLE text-t01.

SELECTION-SCREEN SKIP 1.
PARAMETERS pa_estru RADIOBUTTON GROUP ff.
PARAMETERS pa_campo AS CHECKBOX.

SELECTION-SCREEN SKIP 1.
PARAMETERS pa_geest RADIOBUTTON GROUP ff.

SELECTION-SCREEN SKIP 1.
PARAMETERS pa_sour1 RADIOBUTTON GROUP ff.
" ONB 21/08/2003
PARAMETERS pa_path1 LIKE rlgrap-filename
                   OBLIGATORY DEFAULT 'C:\Datos\XXXXXXXX.csv'.

SELECTION-SCREEN SKIP 1.
PARAMETERS pa_sour2 RADIOBUTTON GROUP ff.
PARAMETERS pa_path2 LIKE rlgrap-filename
                  OBLIGATORY DEFAULT '/sap_divali/'.
SELECTION-SCREEN END OF BLOCK bl001.

* Begin of insertar by ONB 19/03/2005
* Ruta de salida
SELECTION-SCREEN BEGIN OF BLOCK bl004 WITH FRAME TITLE text-t04.
PARAMETERS pa_path3 LIKE rlgrap-filename
                  OBLIGATORY DEFAULT 'C:\TEMP\'.
SELECTION-SCREEN END OF BLOCK bl004.
* End of insertar by ONB 19/03/2005

* Genera Juego de datos
SELECTION-SCREEN BEGIN OF BLOCK bl002 WITH FRAME TITLE text-t02.
PARAMETERS: pa_njdat(12).
SELECTION-SCREEN END OF BLOCK bl002.

*Forma de ejecucion
SELECTION-SCREEN BEGIN OF BLOCK bl003 WITH FRAME TITLE text-t03.
PARAMETERS modo TYPE ctu_mode DEFAULT 'E'.
PARAMETERS act TYPE ctu_update DEFAULT 'A'.
PARAMETERS con AS CHECKBOX.
PARAMETERS nbi AS CHECKBOX.                       " RS  UPGRADE
SELECTION-SCREEN END OF BLOCK bl003.
*****
AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_path1.
  tmp_mask = '*.*,*.*.'.
  tmp_path = 'C:\'.
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = pa_path1
      def_path         = tmp_path
      mask             = tmp_mask
      mode             = '0'
      title            = ' '
    IMPORTING
      filename         = tmp_filename
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.
  IF sy-subrc = 0.
    pa_path1 = tmp_filename.
  ENDIF.
*****
AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_path3.
  tmp_mask = '*.*,*.*.'.
  tmp_path = 'C:\'.
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = pa_path3
      def_path         = tmp_path
      mask             = tmp_mask
      mode             = '0'
      title            = ' '
    IMPORTING
      filename         = tmp_filename
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.
  IF sy-subrc = 0.
    pa_path3 = tmp_filename.
  ENDIF.
*****
START-OF-SELECTION.

  PERFORM cargar_estructura_shdb.
  IF sy-subrc = 0.
    IF pa_estru = 'X'.
      PERFORM mostrar-estructura.
    ELSEIF pa_geest = 'X'.
      PERFORM generar-estructura.
    ELSE.
      PERFORM cargar_archivo_plano.
      PERFORM ejecutar_proceso.
*****
    ENDIF.
  ENDIF.
*---------------------------------------------------------------------*
*       FORM GRABA_INICIO_PANTALLA                                    *
*---------------------------------------------------------------------*
FORM graba_inicio_pantalla.
  ti_bdcdata-program  = ti_shdb-program.
  ti_bdcdata-dynpro   = ti_shdb-dynpro.
  CLEAR ti_bdcdata-fnam.
  CLEAR ti_bdcdata-fval.
  ti_bdcdata-dynbegin = 'X'.
  APPEND ti_bdcdata.
  CLEAR ti_bdcdata.
ENDFORM.                    "graba_inicio_pantalla
*---------------------------------------------------------------------*
*       FORM GRABA_CAMPO_PANTALLA                                     *
*---------------------------------------------------------------------*
FORM graba_campo_pantalla.
  IF ti_shdb-fnam(4) = 'BDC_'.
    ti_bdcdata-fnam     = ti_shdb-fnam.
    ti_bdcdata-fval     = ti_shdb-fval.
    APPEND ti_bdcdata.
    CLEAR ti_bdcdata.
  ELSE.
    ind = ind + 1.
    READ TABLE valsep INDEX ind.
    IF valsep-campos(1) <> '/'.
      ti_bdcdata-fnam     = ti_shdb-fnam.
      ti_bdcdata-fval     = valsep-campos.
      IF valsep-campos(1) = '#'. ti_bdcdata-fval = ' '. ENDIF.
      APPEND ti_bdcdata.
      CLEAR ti_bdcdata.
    ENDIF.
  ENDIF.
ENDFORM.                               " GRABA_CAMPO_PANTALLA
*&---------------------------------------------------------------------*
*&      Form  cargar_estructura_shdb
*&---------------------------------------------------------------------*
FORM cargar_estructura_shdb.
  TABLES: apqi.
  REFRESH ti_shdb.
  SELECT SINGLE qid INTO apqi-qid FROM  apqi
    WHERE  groupid  = pa_shdb
      AND  mandant = sy-mandt.
  CALL FUNCTION 'BDC_OBJECT_READ'
    EXPORTING
      queue_id         = apqi-qid
    TABLES
      dynprotab        = ti_shdb
    EXCEPTIONS
      not_found        = 1
      system_failure   = 2
      invalid_datatype = 3
      OTHERS           = 4.
  IF sy-subrc <> 0.
    MESSAGE s368(00) WITH 'Error en el nombre de la grabación'.
  ELSE.
    READ TABLE ti_shdb INDEX 1.
    IF sy-subrc = 0 AND ti_shdb-dynbegin = 'T'.
      ca_tcode = ti_shdb-fnam.
      DELETE ti_shdb INDEX 1.
    ENDIF.
  ENDIF.
ENDFORM.                    " cargar_estructura_shdb
*&---------------------------------------------------------------------*
*&      Form  cargar_archivo_plano
*&---------------------------------------------------------------------*
FORM cargar_archivo_plano.
  IF pa_sour1 EQ 'X'.
* Inicio AA 08/09/2008 - Migración
*    CALL FUNCTION 'WS_UPLOAD'
*         EXPORTING
*              filename = pa_path1
*              filetype = 'ASC'
*         TABLES
*              data_tab = datos.
    pa_path11 = pa_path1.
    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename                = pa_path11
        filetype                = 'ASC'
      CHANGING
        data_tab                = datos[] " $%11
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19.

* Fin AA 08/09/2008 - Migración
  ELSE.
    OPEN DATASET pa_path2 FOR INPUT IN TEXT MODE ENCODING DEFAULT.
    CHECK ( sy-subrc = 0 ).
    DO.
      READ DATASET pa_path2 INTO datos.
      IF sy-subrc = 4.
        EXIT.
      ENDIF.
      APPEND datos.
    ENDDO.
    CLOSE DATASET pa_path2.
  ENDIF.
ENDFORM.                    " cargar_archivo_plano
*&---------------------------------------------------------------------*
*&      Form  mostrar-estructura
*&---------------------------------------------------------------------*
FORM mostrar-estructura.

  DATA: tabname   TYPE tabname, fieldname TYPE fieldname.
  LOOP AT ti_shdb.
    IF ti_shdb-fnam(4) = 'BDC_' OR ti_shdb-dynbegin = 'X'.
      IF pa_campo IS INITIAL.
        WRITE:/ ti_shdb-program,
                ti_shdb-dynpro,
                ti_shdb-dynbegin,
                ti_shdb-fnam(40),
                ti_shdb-fval(40).
      ENDIF.
    ELSE.
      WRITE:/ ti_shdb-program,
              ti_shdb-dynpro,
              ti_shdb-dynbegin,
              ti_shdb-fnam(40) COLOR 4,
              ti_shdb-fval(40).
      CLEAR: tabname, fieldname.
      SPLIT ti_shdb-fnam AT '-' INTO tabname fieldname.
      SELECT SINGLE * FROM  dd03m
             WHERE  tabname     = tabname
             AND    fieldname   = fieldname
             AND    ddlanguage  = sy-langu.
      IF sy-subrc = 0.
        WRITE: dd03m-scrtext_m.
      ENDIF.

    ENDIF.
  ENDLOOP.
  SKIP 1.
  WRITE:/ 'Transacción:', ca_tcode.
ENDFORM.                    " mostrar-estructura
*&---------------------------------------------------------------------*
*&      Form  ejecutar_proceso
*&---------------------------------------------------------------------*
FORM ejecutar_proceso.
  IF NOT pa_njdat IS INITIAL.
    CALL FUNCTION 'BDC_OPEN_GROUP'
      EXPORTING
        client = sy-mandt
        group  = pa_njdat
        user   = sy-uname
        keep   = 'X'.
  ENDIF.

* Carga Estructura de simulación de pantallas
  LOOP AT datos.
    ind = 0.
    SPLIT datos-valores AT ';' INTO TABLE valsep.
    LOOP AT ti_shdb.
      IF ti_shdb-dynbegin = 'X'.
        PERFORM graba_inicio_pantalla.
      ELSEIF ti_shdb-dynbegin = 'Y' AND solo1vez = 'N'.
        solo1vez = 'Y'.
        PERFORM graba_inicio_pantalla.
      ELSEIF ti_shdb-dynbegin = '1' AND s1 = 'N'.
        s1 = 'Y'.
        PERFORM graba_campo_pantalla.
      ELSEIF ti_shdb-dynbegin = '2' AND s2 = 'N'.
        s2 = 'Y'.
        PERFORM graba_campo_pantalla.
      ELSEIF ti_shdb-dynbegin = '3' AND s3 = 'N'.
        s3 = 'Y'.
        PERFORM graba_campo_pantalla.
      ELSEIF ti_shdb-dynbegin = '4' AND s4 = 'N'.
        s4 = 'Y'.
        PERFORM graba_campo_pantalla.
      ELSEIF ti_shdb-dynbegin = ' '.
        PERFORM graba_campo_pantalla.
      ENDIF.
      CLEAR ti_bdcdata.
    ENDLOOP.
*    IF ACT = 'X'.
*      ACTW = 'S'.
*    ELSE.
*      ACTW = 'N'.
*    ENDIF.

    IF pa_njdat IS INITIAL.
*      CALL TRANSACTION ca_tcode USING ti_BDCDATA
*                                               UPDATE ACT  MODE MODO.
      ctu_params-dismode = modo.
      ctu_params-updmode = act.
      ctu_params-cattmode = ' '.
      ctu_params-defsize = 'X'.
      ctu_params-racommit = con.
*      ctu_params-nobinpt = ' '.           RS  UPGRADE
      ctu_params-nobinpt = nbi.           "RS  UPGRADE
      ctu_params-nobiend = ' '.

      CALL TRANSACTION ca_tcode USING ti_bdcdata
                                OPTIONS FROM ctu_params.
*                                 MESSAGES INTO MESSTAB.




      IF con = 'X'.
        COMMIT WORK.
      ENDIF.
      DELETE  ti_bdcdata WHERE program <> ' ' OR fnam <> ' '.
    ELSE.
      CALL FUNCTION 'BDC_INSERT'
        EXPORTING
          tcode            = ca_tcode
        TABLES
          dynprotab        = ti_bdcdata
        EXCEPTIONS
          internal_error   = 1
          not_open         = 2
          queue_error      = 3
          tcode_invalid    = 4
          printing_invalid = 5
          posting_invalid  = 6
          OTHERS           = 7.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      REFRESH ti_bdcdata.
    ENDIF.
  ENDLOOP.
  IF NOT pa_njdat IS INITIAL.
    CALL FUNCTION 'BDC_CLOSE_GROUP'.
  ENDIF.

ENDFORM.                    " ejecutar_proceso
*&---------------------------------------------------------------------*
*&      Form  Genera la estructura
*&---------------------------------------------------------------------*
FORM generar-estructura.
  DATA: pa_file  LIKE rlgrap-filename,
        pa_file1 TYPE string.
  REFRESH estruc.
  DATA: tabname   TYPE tabname, fieldname TYPE fieldname.
* Campos


  LOOP AT ti_shdb.
    IF ti_shdb-fnam(4) <> 'BDC_' AND ti_shdb-dynbegin IS INITIAL.
      CONCATENATE estruc-valor ti_shdb-fnam(40) INTO estruc-valor
      SEPARATED BY ';'.
    ENDIF.
  ENDLOOP.
  CLEAR  estruc-valor(1).
  estruc-valor = estruc-valor+1.

  APPEND estruc.
  CLEAR estruc.
* Nombre del campo
  LOOP AT ti_shdb.
    IF ti_shdb-fnam(4) <> 'BDC_' AND ti_shdb-dynbegin IS INITIAL.
      CLEAR: tabname, fieldname.
      SPLIT ti_shdb-fnam AT '-' INTO tabname fieldname.
      SELECT SINGLE * FROM  dd03m
             WHERE  tabname     = tabname
             AND    fieldname   = fieldname
             AND    ddlanguage  = sy-langu.
      CONCATENATE estruc-valor dd03m-scrtext_m INTO estruc-valor
      SEPARATED BY ';'.
      CLEAR dd03m-scrtext_m.
    ENDIF.
  ENDLOOP.
  CLEAR  estruc-valor(1).
  estruc-valor = estruc-valor+1.

  APPEND estruc.
  CLEAR estruc.
* Ejemplo de datos
  LOOP AT ti_shdb.
    IF ti_shdb-fnam(4) <> 'BDC_' AND ti_shdb-dynbegin IS INITIAL.
      CONCATENATE estruc-valor ti_shdb-fval(40) INTO estruc-valor
      SEPARATED BY ';'.
    ENDIF.
  ENDLOOP.
  CLEAR  estruc-valor(1).
  estruc-valor = estruc-valor+1.

  APPEND estruc.
  CLEAR estruc.
  LOOP AT estruc.
    WRITE:/ estruc-valor.
  ENDLOOP.

  CONCATENATE pa_path3 pa_shdb '.csv' INTO pa_file.
* concatenate  'C:\TEMP\' pa_shdb '.csv' into pa_file.

* Inicio AA 08/09/2008 - Migración
  pa_file1 = pa_file.
*  CALL FUNCTION 'WS_DOWNLOAD'
*    EXPORTING
*      filename                = pa_file
*      filetype                = 'DAT'
*    TABLES
*      data_tab                = estruc
*    EXCEPTIONS
*      file_open_error         = 1
*      file_write_error        = 2
*      invalid_filesize        = 3
*      invalid_type            = 4
*      no_batch                = 5
*      unknown_error           = 6
*      invalid_table_width     = 7
*      gui_refuse_filetransfer = 8
*      customer_error          = 9
*      OTHERS                  = 10.
  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      filename                = pa_file1
      filetype                = 'DAT'
    CHANGING
      data_tab                = estruc[]
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      not_supported_by_gui    = 22
      error_no_gui            = 23
      OTHERS                  = 24.

* Fin AA 08/09/2008 - Migración
  CASE sy-subrc.
    WHEN '0'.
      MESSAGE s998(z1) WITH 'Se genero el archivo' pa_file
                        '. Utilizar Excel para abrir.'.
    WHEN '2'.
      CONCATENATE text-e01 pa_path3 ' por favor' INTO v_text.
      MESSAGE e998(z1) WITH v_text text-e02.
    WHEN OTHERS.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDCASE.


ENDFORM.                    " generar-estructura

