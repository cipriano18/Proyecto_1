      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. proyecto.
       ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
       SELECT ARCH-PROD ASSIGN TO "productos.dat"
        ORGANIZATION IS INDEXED
        ACCESS MODE IS DYNAMIC
        RECORD KEY IS PRD-CODIGO
        FILE STATUS IS FS-PROD.

       SELECT ARCH-VEN ASSIGN TO "ventas.txt"
            ORGANIZATION IS  SEQUENTIAL
            ACCESS MODE IS SEQUENTIAL
            FILE STATUS IS FS-VEN.
       DATA DIVISION.
       FILE SECTION.
        FD  ARCH-PROD.
        01  REG-PROD.
           02 PRD-CODIGO    PIC X(5).
           02 PRD-NOMBRE    PIC X(25).
           02 PRD-CATEGORIA PIC X(15).
           02 PRD-PRECIO    PIC 9(5)V99.
           02 PRD-STOCK     PIC 9(5).

       FD  ARCH-VEN.
       01  REG-VENTA-LINE  PIC X(80).
       WORKING-STORAGE SECTION.
       01  FS-PROD           PIC XX VALUE "00".
       01  FS-VEN            PIC XX VALUE "00".

       01  OPCION            PIC 9      VALUE 0.
       01  OPCION-TEXTO      PIC X(10)  VALUE SPACES.
       01  PAUSA-TEXTO       PIC X(1)   VALUE SPACES.

       *> Entradas de producto (texto)
       01  IN-PROD.
           02 IN-CODIGO       PIC X(5).
           02 IN-NOMBRE       PIC X(25).
           02 IN-CATEGORIA    PIC X(15).
           02 IN-PRECIO-TX    PIC X(20).
           02 IN-STOCK-TX     PIC X(10).

       *>-- Trabajo numérico general
       01  WS-PRECIO          PIC 9(5)V99   VALUE 0.
       01  WS-STOCK           PIC 9(5)      VALUE 0.
       01  WS-PRECIO-SIGN     PIC S9(7)V99  VALUE 0.
       01  WS-STOCK-SIGN      PIC S9(7)     VALUE 0.
       01  WS-VALIDO          PIC X         VALUE "S".
       01  WS-OK-PRECIO       PIC X         VALUE "S".
       01  WS-OK-STOCK        PIC X         VALUE "S".
       01  WS-CANT-SIGN       PIC S9(7)     VALUE 0.
       01  WS-TOTAL           PIC 9(9)V99   VALUE 0.

       *>-- Campos editados para DISPLAY (listas generales)
       01  WS-PRECIO-ED       PIC ZZZZ9.99.
       01  WS-STOCK-ED        PIC ZZZZ9.

       *>-- Entradas de venta (texto)
       01  IN-VENTA.
           02 IN-CODIGO-VEN   PIC X(5).
           02 IN-CANTIDAD-TX  PIC X(10).
           02 IN-FECHA-VEN    PIC X(8).

       *>-- Manejo/validación de fecha
       01  WS-FECHA-TRIM      PIC X(8).
       01  WS-ANO             PIC 9(4).
       01  WS-MES             PIC 9(2).
       01  WS-DIA             PIC 9(2).
       01  WS-MAX-DIA         PIC 9(2).

       *>-- Reporte por categoría
       01  IN-CATEG-FILT      PIC X(15)      VALUE SPACES.
       01  WS-CONT-CAT        PIC 9(5)       VALUE 0.
       01  WS-VALOR-LINE      PIC 9(11)V99   VALUE 0.
       01  WS-VALOR-CAT       PIC 9(13)V99   VALUE 0.
       01  WS-VALOR-ED        PIC ZZZZZZ9.99.
       01  WS-VALOR-CAT-ED    PIC ZZZZZZZZZ9.99.
       01  WS-CONT-CAT-ED     PIC ZZZZZ9.

       *>-- Reporte de ventas por rango de fechas
       01  IN-FECHA-INI       PIC X(8)       VALUE SPACES.
       01  IN-FECHA-FIN       PIC X(8)       VALUE SPACES.

       *> Campos para parsear cada línea de ventas (REG-VENTA-LINE)
       01  V-COD              PIC X(5).
       01  V-CANT-TX          PIC X(10).
       01  V-FECHA            PIC X(8).
       01  V-CANT             PIC 9(7)       VALUE 0.

       *> Acumuladores y “más vendido”
       01  WS-TOTAL-ING       PIC 9(13)V99   VALUE 0.
       01  WS-MAX-CANT        PIC 9(7)       VALUE 0.
       01  WS-MAX-COD         PIC X(5)       VALUE SPACES.
       01  WS-MAX-NOM         PIC X(25)      VALUE SPACES.

       *> Editados para mostrar bonitos en el reporte de ventas
       01  V-CANT-ED        PIC Z(7).
       01  WS-IMP-ED        PIC ZZZZZZZZZ9.99.
       01  WS-TOTAL-ING-ED  PIC ZZZZZZZZZ9.99.
       01  WS-MAX-CANT-ED   PIC Z(7).


       PROCEDURE DIVISION.
          MAIN-START.
          OPEN I-O ARCH-PROD
         IF FS-PROD = "35"
        OPEN OUTPUT ARCH-PROD
        CLOSE ARCH-PROD
        OPEN I-O ARCH-PROD
         END-IF

        OPEN EXTEND ARCH-VEN
         IF FS-VEN = "35"
        OPEN OUTPUT ARCH-VEN
        CLOSE ARCH-VEN
        OPEN EXTEND ARCH-VEN
         END-IF

            PERFORM MENU

          CLOSE ARCH-PROD
        CLOSE ARCH-VEN
       STOP RUN.


       MENU.
           PERFORM CICLO
               UNTIL OPCION = 7.

       CICLO.
       DISPLAY "---------------------------------------------".
         DISPLAY " Sistema de Gestion de Inventario y Ventas".
       DISPLAY "---------------------------------------------".
            DISPLAY "1) Registrar nuevo producto".
            DISPLAY "2) Modificar datos de un producto".
            DISPLAY "3) Registrar venta".
            DISPLAY "4) Reporte general de productos".
            DISPLAY "5) Reporte por categoria".
            DISPLAY "6) Reporte de ventas por fechas".
            DISPLAY "7) Salir".
            DISPLAY "Seleccione una opcion  "
            ACCEPT OPCION-TEXTO

        IF OPCION-TEXTO = SPACES
        MOVE 0 TO OPCION
        ELSE
        MOVE FUNCTION NUMVAL(OPCION-TEXTO) TO OPCION
        END-IF

        EVALUATE OPCION
        WHEN 1
           PERFORM REGISTRAR-PRODUCTO
        WHEN 2
           PERFORM MODIFICAR-PRODUCTO
        WHEN 3
           PERFORM REGISTRAR-VENTA
        WHEN 4
           PERFORM LISTAR-PRODUCTOS
        WHEN 5
            PERFORM REPORTE-POR-CATEGORIA
        WHEN 6
           PERFORM REPORTE-VENTAS-RANGO
        WHEN 7
            DISPLAY "Saliendo..."
        WHEN OTHER
            DISPLAY "Opcion invalida."
       END-EVALUATE.
       REGISTRAR-PRODUCTO.
        MOVE "S" TO WS-OK-PRECIO WS-OK-STOCK

            DISPLAY "Ingrese el codigo del producto "
           ACCEPT  IN-CODIGO
        MOVE FUNCTION TRIM(IN-CODIGO) TO PRD-CODIGO
             IF LENGTH OF PRD-CODIGO NOT = 5
              DISPLAY "Codigo invalido debe tener 5 caracteres"
               EXIT PARAGRAPH
        MOVE "N" TO WS-OK-PRECIO
        MOVE "N" TO WS-OK-STOCK
            ELSE
            READ ARCH-PROD KEY IS PRD-CODIGO
            INVALID KEY CONTINUE
            NOT INVALID KEY
                DISPLAY "Ya existe un producto con ese codigo"
                MOVE "N" TO WS-OK-PRECIO
                MOVE "N" TO WS-OK-STOCK
                 EXIT PARAGRAPH
           END-READ
               END-IF

             DISPLAY "Ingrese el nombre del producto "
             ACCEPT  IN-NOMBRE
             MOVE FUNCTION TRIM(IN-NOMBRE)(1:25) TO PRD-NOMBRE
           IF PRD-NOMBRE = SPACES
             DISPLAY "Nombre invalido no puede estar vacio"
        MOVE "N" TO WS-OK-PRECIO
        MOVE "N" TO WS-OK-STOCK
             END-IF

            DISPLAY "Ingrese la categoria del producto"
            ACCEPT  IN-CATEGORIA
        MOVE FUNCTION TRIM(IN-CATEGORIA)(1:15) TO PRD-CATEGORIA
             IF PRD-CATEGORIA = SPACES
            DISPLAY "Categoria invalida no puede estar vacia"
        MOVE "N" TO WS-OK-PRECIO
        MOVE "N" TO WS-OK-STOCK
            END-IF

            DISPLAY "Ingrese el precio del producto "
          ACCEPT IN-PRECIO-TX
            IF FUNCTION TEST-NUMVAL-F(IN-PRECIO-TX) NOT = 0
             DISPLAY "Precio invalido (formato)."
        MOVE "N" TO WS-OK-PRECIO
            ELSE
        MOVE FUNCTION NUMVAL(IN-PRECIO-TX) TO WS-PRECIO-SIGN
             IF WS-PRECIO-SIGN <= 0
            DISPLAY "Precio invalido (debe ser > 0)."
        MOVE "N" TO WS-OK-PRECIO
           END-IF
            END-IF

            DISPLAY "Ingrese la cantidad de productos: "
            ACCEPT IN-STOCK-TX
               IF FUNCTION TEST-NUMVAL(IN-STOCK-TX) NOT = 0
               DISPLAY "Stock invalido (solo numeros)."
        MOVE "N" TO WS-OK-STOCK
           ELSE
        MOVE FUNCTION NUMVAL(IN-STOCK-TX) TO WS-STOCK-SIGN
             IF WS-STOCK-SIGN < 0
            DISPLAY "Stock invalido (debe ser >= 0)."
            MOVE "N" TO WS-OK-STOCK
            ELSE
            IF FUNCTION INTEGER(WS-STOCK-SIGN) NOT = WS-STOCK-SIGN
                DISPLAY "El stock debe ser entero."
                MOVE "N" TO WS-OK-STOCK
            END-IF
            END-IF
            END-IF
              IF WS-OK-PRECIO = "S" AND WS-OK-STOCK = "S"
        MOVE WS-PRECIO-SIGN TO PRD-PRECIO
        MOVE WS-STOCK-SIGN  TO PRD-STOCK
        WRITE REG-PROD
            INVALID KEY DISPLAY "Clave duplicada; no se puede registrar"
            NOT INVALID KEY DISPLAY "Producto registrado."
        END-WRITE
         ELSE
        DISPLAY "Registro cancelado por datos invalidos."
        END-IF.

       MODIFICAR-PRODUCTO.
        MOVE "S" TO WS-OK-PRECIO WS-OK-STOCK

            DISPLAY "Ingrese el codigo del producto a modificar "
            ACCEPT  IN-CODIGO
        MOVE FUNCTION TRIM(IN-CODIGO) TO PRD-CODIGO

            READ ARCH-PROD KEY IS PRD-CODIGO
            INVALID KEY
            DISPLAY "No existe un producto con ese codigo."
            EXIT PARAGRAPH
            END-READ

        MOVE PRD-PRECIO TO WS-PRECIO-ED
        MOVE PRD-STOCK  TO WS-STOCK-ED
             DISPLAY "Actual:"
            DISPLAY "  Nombre:    " FUNCTION TRIM(PRD-NOMBRE)
             DISPLAY "  Categoria: " FUNCTION TRIM(PRD-CATEGORIA)
            DISPLAY "  Precio:    " FUNCTION TRIM(WS-PRECIO-ED)
            DISPLAY "  Stock:     " FUNCTION TRIM(WS-STOCK-ED)
            DISPLAY "Deje en blanco para mantener el valor actual."

             DISPLAY "Nuevo nombre del producto "
            ACCEPT  IN-NOMBRE
             IF IN-NOMBRE NOT = SPACES
        MOVE FUNCTION TRIM(IN-NOMBRE)(1:25) TO PRD-NOMBRE
           END-IF

            DISPLAY "Nueva categoria del producto "
             ACCEPT  IN-CATEGORIA
            IF IN-CATEGORIA NOT = SPACES
        MOVE FUNCTION TRIM(IN-CATEGORIA)(1:15) TO PRD-CATEGORIA
           END-IF

            DISPLAY "Nuevo precio del producto "
                ACCEPT  IN-PRECIO-TX
             IF IN-PRECIO-TX NOT = SPACES
             IF FUNCTION TEST-NUMVAL-F(IN-PRECIO-TX) NOT = 0
            DISPLAY "Precio invalido (formato)."
            MOVE "N" TO WS-OK-PRECIO
             ELSE
            MOVE FUNCTION NUMVAL(IN-PRECIO-TX) TO WS-PRECIO-SIGN
            IF WS-PRECIO-SIGN > 0
                MOVE WS-PRECIO-SIGN TO PRD-PRECIO
            ELSE
                DISPLAY "Precio debe ser > 0. Actual no cambia."
                MOVE "N" TO WS-OK-PRECIO
              END-IF
             END-IF
           END-IF

            DISPLAY "Nuevo stock del producto "
             ACCEPT  IN-STOCK-TX
             IF IN-STOCK-TX NOT = SPACES
              IF FUNCTION TEST-NUMVAL(IN-STOCK-TX) NOT = 0
            DISPLAY "Stock invalido (formato)."
         MOVE "N" TO WS-OK-STOCK
              ELSE
            MOVE FUNCTION NUMVAL(IN-STOCK-TX) TO WS-STOCK-SIGN
            IF WS-STOCK-SIGN < 0
                DISPLAY "Stock debe ser >= 0. Actual no cambia."
                MOVE "N" TO WS-OK-STOCK
            ELSE
                IF FUNCTION INTEGER(WS-STOCK-SIGN) = WS-STOCK-SIGN
                    MOVE WS-STOCK-SIGN TO PRD-STOCK
                ELSE
                    DISPLAY "Stock debe ser entero. Actual no cambia."
                    MOVE "N" TO WS-OK-STOCK
                END-IF
            END-IF
        END-IF
       END-IF


          IF WS-OK-PRECIO = "S" AND WS-OK-STOCK = "S"
        REWRITE REG-PROD
            INVALID KEY
             DISPLAY "ERROR: no se pudo guardar los cambios (REWRITE)."
            NOT INVALID KEY
                DISPLAY "Producto actualizado correctamente."
        END-REWRITE
       ELSE
        DISPLAY "Actualizacion cancelada por datos invalidos."
           END-IF.
       REGISTRAR-VENTA.
            DISPLAY "----- Registrar venta -----".
         DISPLAY "Ingrese el codigo del producto a vender: "
           ACCEPT PRD-CODIGO

          READ ARCH-PROD KEY IS PRD-CODIGO
        INVALID KEY
            DISPLAY "ERROR: Ese codigo de producto no existe."
            EXIT PARAGRAPH
          END-READ

         DISPLAY "Ingrese cantidad vendida (entero > 0): "
           ACCEPT IN-CANTIDAD-TX

           IF FUNCTION TEST-NUMVAL(IN-CANTIDAD-TX) NOT = 0
        DISPLAY "ERROR: Cantidad invalida."
        EXIT PARAGRAPH
            END-IF
        MOVE FUNCTION NUMVAL(IN-CANTIDAD-TX) TO WS-CANT-SIGN
         IF WS-CANT-SIGN <= 0 OR
       FUNCTION INTEGER(WS-CANT-SIGN) NOT = WS-CANT-SIGN
        DISPLAY "ERROR: La cantidad debe ser un entero positivo."
        EXIT PARAGRAPH
          END-IF

          IF WS-CANT-SIGN > PRD-STOCK
        DISPLAY "ERROR: No hay suficiente stock. Disponible: " PRD-STOCK
        EXIT PARAGRAPH
          END-IF

           *> --- Fecha AAAAMMDD (sin bisiestos) ---
         DISPLAY "Fecha de la venta (AAAAMMDD): "
          ACCEPT IN-FECHA-VEN

          MOVE FUNCTION TRIM(IN-FECHA-VEN) TO WS-FECHA-TRIM

         IF FUNCTION LENGTH(WS-FECHA-TRIM) NOT = 8
        DISPLAY "ERROR: Fecha invalida (8 digitos AAAAMMDD)."
        EXIT PARAGRAPH
          END-IF
         IF FUNCTION TEST-NUMVAL(WS-FECHA-TRIM) NOT = 0
        DISPLAY "ERROR: Fecha invalida (solo numeros)."
        EXIT PARAGRAPH
          END-IF

       COMPUTE WS-ANO = FUNCTION NUMVAL(WS-FECHA-TRIM(1:4))
         COMPUTE WS-MES = FUNCTION NUMVAL(WS-FECHA-TRIM(5:2))
         COMPUTE WS-DIA = FUNCTION NUMVAL(WS-FECHA-TRIM(7:2))

       IF WS-MES < 1 OR WS-MES > 12
        DISPLAY "ERROR: Mes fuera de rango (01-12)."
        EXIT PARAGRAPH
        END-IF
        EVALUATE WS-MES
       WHEN 1
         WHEN 3
        WHEN 5
         WHEN 7
           WHEN 8
           WHEN 10
             WHEN 12
       MOVE 31 TO WS-MAX-DIA

             WHEN 4
            WHEN 6
           WHEN 9
           WHEN 11
       MOVE 30 TO WS-MAX-DIA

          WHEN 2
       MOVE 28 TO WS-MAX-DIA   *> sin bisiesto
         END-EVALUATE

       IF WS-DIA < 1 OR WS-DIA > WS-MAX-DIA
        DISPLAY "ERROR: Dia fuera de rango para el mes."
        EXIT PARAGRAPH
          END-IF
         *> --- fin validacion fecha ---

       *> Construir linea: CODIGO|CANTIDAD|FECHA
          MOVE SPACES TO REG-VENTA-LINE
           STRING
        PRD-CODIGO                    DELIMITED BY SIZE
        "|"                           DELIMITED BY SIZE
        FUNCTION TRIM(IN-CANTIDAD-TX) DELIMITED BY SIZE
        "|"                           DELIMITED BY SIZE
        WS-FECHA-TRIM                 DELIMITED BY SIZE
        INTO REG-VENTA-LINE
        END-STRING

          *> Escribir primero en ventas
        WRITE REG-VENTA-LINE
          IF FS-VEN NOT = "00"
        DISPLAY "ERROR al escribir en ventas.txt. FS=" FS-VEN
        EXIT PARAGRAPH
       END-IF

         *> Solo si la venta se escribio bien, actualizar stock
          SUBTRACT WS-CANT-SIGN FROM PRD-STOCK
           REWRITE REG-PROD
        INVALID KEY
       DISPLAY "ERROR: Venta guardada pero no se pudo actualizar stock."
            ADD WS-CANT-SIGN TO PRD-STOCK
            EXIT PARAGRAPH
         END-REWRITE

             DISPLAY "Venta registrada: " PRD-CODIGO "|"
            FUNCTION TRIM(IN-CANTIDAD-TX) "|" WS-FECHA-TRIM
        .

        LISTAR-PRODUCTOS.
           DISPLAY"   "
            DISPLAY "Productos"
        MOVE SPACES TO PRD-CODIGO
         START ARCH-PROD KEY NOT LESS THAN PRD-CODIGO
        INVALID KEY
            DISPLAY "No hay productos registrados."
            EXIT PARAGRAPH
       END-START

       PERFORM UNTIL FS-PROD NOT = "00"
        READ ARCH-PROD NEXT RECORD
            AT END
                EXIT PERFORM
                NOT AT END
        MOVE PRD-PRECIO TO WS-PRECIO-ED
         MOVE PRD-STOCK  TO WS-STOCK-ED
           DISPLAY FUNCTION TRIM(PRD-CODIGO) "|"
            FUNCTION TRIM(PRD-NOMBRE) "|"
            FUNCTION TRIM(PRD-CATEGORIA) "|"
            FUNCTION TRIM(WS-PRECIO-ED) "|"
            FUNCTION TRIM(WS-STOCK-ED)
       END-READ
       END-PERFORM.
       REPORTE-POR-CATEGORIA.
          DISPLAY "Ingrese la categoria a filtrar: "
          ACCEPT IN-CATEG-FILT
          MOVE FUNCTION TRIM(IN-CATEG-FILT)     TO IN-CATEG-FILT
          MOVE FUNCTION UPPER-CASE(IN-CATEG-FILT) TO IN-CATEG-FILT

          MOVE 0 TO WS-CONT-CAT
          MOVE 0 TO WS-VALOR-CAT

          DISPLAY " "
          DISPLAY "REPORTE POR CATEGORIA: " IN-CATEG-FILT
         DISPLAY "COD  |NOMBRE  |CATEGORIA      |PRECIO   |STOCK |VALOR"
          DISPLAY "-------------------------------------------"

          MOVE SPACES TO PRD-CODIGO
          START ARCH-PROD KEY NOT LESS THAN PRD-CODIGO
             INVALID KEY
                DISPLAY "No hay productos registrados."
                EXIT PARAGRAPH
          END-START

          PERFORM UNTIL FS-PROD NOT = "00"
             READ ARCH-PROD NEXT RECORD
                AT END
                   EXIT PERFORM
                NOT AT END
                   IF FUNCTION UPPER-CASE(FUNCTION TRIM(PRD-CATEGORIA))
                      = IN-CATEG-FILT
                      COMPUTE WS-VALOR-LINE = PRD-PRECIO * PRD-STOCK
                      ADD 1             TO WS-CONT-CAT
                      ADD WS-VALOR-LINE TO WS-VALOR-CAT

                      MOVE PRD-PRECIO    TO WS-PRECIO-ED
                      MOVE PRD-STOCK     TO WS-STOCK-ED
                      MOVE WS-VALOR-LINE TO WS-VALOR-ED

                      DISPLAY PRD-CODIGO " |"
                              FUNCTION TRIM(PRD-NOMBRE)(1:24) " |"
                              FUNCTION TRIM(PRD-CATEGORIA)(1:12) " |"
                              WS-PRECIO-ED " |"
                              WS-STOCK-ED  " |"
                              WS-VALOR-ED
                   END-IF
             END-READ
          END-PERFORM

          DISPLAY "----------------------------------------------------"
          IF WS-CONT-CAT = 0
             DISPLAY "No hay productos en esa categoria."
          ELSE
             MOVE WS-CONT-CAT  TO WS-CONT-CAT-ED
             MOVE WS-VALOR-CAT TO WS-VALOR-CAT-ED
             DISPLAY "TOTAL PRODUCTOS EN CATEGORIA: " WS-CONT-CAT-ED
             DISPLAY "VALOR TOTAL CATEGORIA: " WS-VALOR-CAT-ED
          END-IF
          .
       REPORTE-VENTAS-RANGO.
           CLOSE ARCH-VEN
        OPEN INPUT ARCH-VEN

         MOVE 0 TO WS-TOTAL-ING WS-MAX-CANT

       DISPLAY "Fecha inicio (AAAAMMDD): "
        ACCEPT IN-FECHA-INI

        *> Validar IN-FECHA-INI reutilizando tus mismas variables/bloque
       MOVE FUNCTION TRIM(IN-FECHA-INI) TO WS-FECHA-TRIM
          IF FUNCTION LENGTH(WS-FECHA-TRIM) NOT = 8
        DISPLAY "ERROR: Fecha inicio invalida (8 digitos)."
        CLOSE ARCH-VEN
        OPEN EXTEND ARCH-VEN
        EXIT PARAGRAPH
        END-IF
         IF FUNCTION TEST-NUMVAL(WS-FECHA-TRIM) NOT = 0
        DISPLAY "ERROR: Fecha inicio invalida (solo numeros)."
        CLOSE ARCH-VEN
        OPEN EXTEND ARCH-VEN
        EXIT PARAGRAPH
        END-IF
       COMPUTE WS-ANO = FUNCTION NUMVAL(WS-FECHA-TRIM(1:4))
        COMPUTE WS-MES = FUNCTION NUMVAL(WS-FECHA-TRIM(5:2))
         COMPUTE WS-DIA = FUNCTION NUMVAL(WS-FECHA-TRIM(7:2))
       IF WS-MES < 1 OR WS-MES > 12
        DISPLAY "ERROR: Mes inicio fuera de rango."
        CLOSE ARCH-VEN
        OPEN EXTEND ARCH-VEN
        EXIT PARAGRAPH
       END-IF
       EVALUATE WS-MES
       WHEN 1 WHEN 3 WHEN 5 WHEN 7 WHEN 8 WHEN 10 WHEN 12
            MOVE 31 TO WS-MAX-DIA
       WHEN 4 WHEN 6 WHEN 9 WHEN 11
            MOVE 30 TO WS-MAX-DIA
       WHEN 2
            MOVE 28 TO WS-MAX-DIA
          END-EVALUATE
         IF WS-DIA < 1 OR WS-DIA > WS-MAX-DIA
        DISPLAY "ERROR: Dia inicio fuera de rango."
        CLOSE ARCH-VEN
        OPEN EXTEND ARCH-VEN
         EXIT PARAGRAPH
          END-IF

        DISPLAY "Fecha fin (AAAAMMDD): "
         ACCEPT IN-FECHA-FIN

        *> Validar IN-FECHA-FIN con el mismo bloque
         MOVE FUNCTION TRIM(IN-FECHA-FIN) TO WS-FECHA-TRIM
            IF FUNCTION LENGTH(WS-FECHA-TRIM) NOT = 8
        DISPLAY "ERROR: Fecha fin invalida (8 digitos)."
        CLOSE ARCH-VEN
        OPEN EXTEND ARCH-VEN
        EXIT PARAGRAPH
          END-IF
          IF FUNCTION TEST-NUMVAL(WS-FECHA-TRIM) NOT = 0
        DISPLAY "ERROR: Fecha fin invalida (solo numeros)."
        CLOSE ARCH-VEN
        OPEN EXTEND ARCH-VEN
        EXIT PARAGRAPH
        END-IF
         COMPUTE WS-ANO = FUNCTION NUMVAL(WS-FECHA-TRIM(1:4))
           COMPUTE WS-MES = FUNCTION NUMVAL(WS-FECHA-TRIM(5:2))
        COMPUTE WS-DIA = FUNCTION NUMVAL(WS-FECHA-TRIM(7:2))
         IF WS-MES < 1 OR WS-MES > 12
        DISPLAY "ERROR: Mes fin fuera de rango."
        CLOSE ARCH-VEN
        OPEN EXTEND ARCH-VEN
        EXIT PARAGRAPH
        END-IF
           EVALUATE WS-MES
       WHEN 1 WHEN 3 WHEN 5 WHEN 7 WHEN 8 WHEN 10 WHEN 12
            MOVE 31 TO WS-MAX-DIA
       WHEN 4 WHEN 6 WHEN 9 WHEN 11
            MOVE 30 TO WS-MAX-DIA
       WHEN 2
            MOVE 28 TO WS-MAX-DIA
         END-EVALUATE
         IF WS-DIA < 1 OR WS-DIA > WS-MAX-DIA
        DISPLAY "ERROR: Dia fin fuera de rango."
        CLOSE ARCH-VEN
        OPEN EXTEND ARCH-VEN
        EXIT PARAGRAPH
          END-IF

           IF IN-FECHA-INI > IN-FECHA-FIN
        DISPLAY "ERROR: Fecha inicio mayor que fecha fin."
        CLOSE ARCH-VEN
        OPEN EXTEND ARCH-VEN
        EXIT PARAGRAPH
        END-IF

        DISPLAY " "
       DISPLAY "REPORTE DE VE " IN-FECHA-INI " HASTA " IN-FECHA-FIN.
       DISPLAY "COD  NOMBRE   CANTIDAD  TOTAL_VENTA"
       DISPLAY "------------------------------------------------------"

       PERFORM UNTIL 1 = 2
        READ ARCH-VEN NEXT RECORD
            AT END
                EXIT PERFORM
            NOT AT END
                UNSTRING REG-VENTA-LINE DELIMITED BY "|"
                         INTO V-COD V-CANT-TX V-FECHA
                END-UNSTRING

                MOVE FUNCTION TRIM(V-FECHA) TO V-FECHA

                IF V-FECHA >= IN-FECHA-INI AND V-FECHA <= IN-FECHA-FIN
                    MOVE FUNCTION NUMVAL(V-CANT-TX) TO V-CANT

                    *> Buscar producto para nombre y precio
                    MOVE V-COD TO PRD-CODIGO
                    READ ARCH-PROD KEY IS PRD-CODIGO
                        INVALID KEY
                            CONTINUE
                        NOT INVALID KEY
                            COMPUTE WS-VALOR-LINE = PRD-PRECIO * V-CANT
                            ADD WS-VALOR-LINE TO WS-TOTAL-ING

                            IF V-CANT > WS-MAX-CANT
                                MOVE V-CANT     TO WS-MAX-CANT
                                MOVE PRD-CODIGO TO WS-MAX-COD
                                MOVE PRD-NOMBRE TO WS-MAX-NOM
                            END-IF
                            MOVE V-CANT         TO V-CANT-ED
                            MOVE WS-VALOR-LINE  TO WS-IMP-ED

                            DISPLAY PRD-CODIGO " "
                                    FUNCTION TRIM(PRD-NOMBRE)(1:25) "  "
                                    V-CANT-ED "    "
                                    WS-IMP-ED

                    END-READ
                END-IF
        END-READ
        END-PERFORM
       MOVE WS-TOTAL-ING TO WS-TOTAL-ING-ED
       DISPLAY "------------------------------------------------------"
       DISPLAY "TOTAL INGRESOS: " WS-TOTAL-ING-ED

       IF WS-MAX-CANT > 0
          MOVE WS-MAX-CANT TO WS-MAX-CANT-ED
          DISPLAY "PRODUCTO MAS VENDIDO: " FUNCTION TRIM(WS-MAX-NOM)
                  " (" WS-MAX-CANT-ED " unidades)"
       ELSE
          DISPLAY "No hubo ventas en el rango."
       END-IF


            CLOSE ARCH-VEN
         OPEN EXTEND ARCH-VEN
         .


       END PROGRAM proyecto.
