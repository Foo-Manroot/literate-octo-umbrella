/* -------------------------------------- */
/* Funciones para procesar los argumentos */
/* -------------------------------------- */
package utils

import malla.Partida
import scala.util.matching.Regex

object Utils {

  val ayuda = "Uso: \n" +
              "candy <nivel_juego>\n"
  /**
   * Comprueba la condición especificada y ejecuta el código de la rama que corresponda.
   * Para ejecutar, por ejemplo, lo siguiente:
   *    if (2 > 3) {
   *
   *        println ("Mayor")
   *    } else {
   *
   *        println ("Menor")
   *    }
   *
   * Se podría usar esta función de la siguiente manera:
   *
   *    if_else (2 > 3, () => println ("Mayor"), () => println ("Mayor"))
   *
   *
   * @param cond
   *          Condición a comprobar.
   *
   * @param cod_if
   *          Código a ejecutar si la condición es cierta
   *
   * @param cod_else
   *          Código a ejecutar si la condición es falsa. Si no se desea ejecutar nada,
   *        este parámetro puede omitirse.
   */
  def if_else[T] (cond: Boolean, cod_if: () => T, cod_else: () => T = () => ()): T = {

    if ( (cond) ) {

      cod_if ()
    } else {

      cod_else ()
    }
  }

  /**
   * Inserta un 0 en la posición dada.
   *
   * @param pos
   *          Posición en la que se debe introducir el elemento
   *
   * @param lista
   *          Lista en la que se debe introducir el elemento
   *
   *
   * @return
   *          Una nueva lista con un 0 en la posición especificada
   */
  def borrar (pos: Int, lista: List [Any]): List [Any] = {

    insertar (0, pos, lista)
  }

  /**
   * Inserta el elemento en una posición de la lista.
   *
   *
   * @param color
   *          Elemento que se quiere añadir
   *
   * @param pos
   *          Posición del elemento a añadir
   *
   * @param lista
   *          Lista en la que añadir el elemento
   *
   *
   * @return
   *        Una lista con el elemento, sustituyendo el elemento de la posición "pos"
   */
  def insertar (color: Any, pos: Int, lista: List [Any]): List [Any] = {

    if (pos == 0)
      color::lista.tail
    else
      lista.head::insertar (color, pos - 1, lista.tail)
  }


  /**
   * Crea una matriz de n enteros aleatorios
   *
   * @param n
   *          Número de elementos a crear
   *
   * @param nivel
   *          Nivel del juego para limitar el rango de los diamantes a crear
   *
   * @return
   *          Lista con los elementos
   */
  def crear_lista (n: Int, nivel: Int): List [Int] = {

    if (n <= 0) Nil else crear_diamante (nivel)::crear_lista (n - 1, nivel)
  }

  /**
   * Crea un nuevo diamante con valor aleatorio según el nivel
   *
   * @param nivel
   *          Nivel de juego que se va a usar para limitar el rango del diamante a crear
   *
   * @return
   *          Un entero para representar un diamante
   */
  def crear_diamante (nivel: Int): Int = {

    nivel match {

      case 1 => (util.Random.nextInt (4) + 1)
      case 2 => (util.Random.nextInt (5) + 1)
      case 3 => (util.Random.nextInt (7) + 1)
      case _ => (util.Random.nextInt (7) + 1)
    }
  }


  /**
   * Obtiene el único argumento aceptado (el nivel de juego) y saca la información
   * del juego.
   *
   * @param args
   *            Array con los argumentos pasados al programa
   *
   * @return
   *            Un objeto con la información de la partida (filas, columnas y nivel)
   */
  def obtener_args (args: Array [String]): Partida = {

    if_else (args.length != 1,
              () => {
                print (ayuda)
                sys.exit (1)
              },
              () => comprobar_arg_nv(args (0))
    )
  }

  /**
   * Pide la entrada por teclado en bucle hasta que se introduce una de las opciones
   * válidas.
   * Si las opciones aceptadas son [0, 1, 2, 3, 4], la llamada a esta función sería
   *  pedir_opción (0, 4)
   *
   * @param min
   *          Primera opción del rango (inclusive)
   *
   * @param max
   *          Última opción del rango (inclusive)
   *
   * @return
   *        El valor de la opción seleccionada
   */
  def pedir_opción (min: Int, max: Int): Int = {

    val entrada = scala.io.StdIn.readLine ();

    if_else (esNum (entrada)
              && (entrada.toInt >= min)
              && (entrada.toInt <= max),
              () => entrada.toInt,
              () => {

                print ("Opción no válida.\n" +
                     "Introduzca un número en el rango [" + min + ", " + max + "]: ")
                pedir_opción (min, max)
              }
    )
  }

  /**
   * Comprueba si el argumento es un nivel válido. Si lo es, devuelve un objeto de tipo
   * malla inicializado correctamente.
   *
   * @param arg
   *          El argumento a comprobar
   *
   * @return
   *          Un objeto de tipo Partida si el argumento es correcto. Si no, sale
   *        del programa.
   */
  private def comprobar_arg_nv (arg: String): Partida = {

    if_else (esNum (arg),
              () => try{

                Factoría.crear_partida (arg.toInt)
              } catch {

                  case e: Throwable => { log_error (e.getMessage ()); sys.exit (-1) }
              },
              () => {

                log_error ("El argumento especificado, '"+ arg +
                            "' no es un número válido");
                sys.exit (-1)
              }
    )
  }

  /**
   * Imprime por pantalla el mensaje de error ya formateado.
   *
   * @param msg
   *        Mensaje de error a imprimir.
   */
  def log_error (msg: String): Unit = { print (" --> Error: " + msg + "\n") }

  /**
   * Comprueba si la cadena es un número.
   *
   * @param x
   *        Cadena que se desea comprobar
   *
   * @return
   *        true si todos los elementos de la cadena son dígitos; o false si no.
   */
  def esNum (x: String) = !x.isEmpty && x.forall (Character.isDigit);

  /**
   * Comprueba si tres elementos están en la misma fila y de manera consecutiva
   *
   * @param A
   *          Índice del primer elemento de la serie
   *
   * @param B
   *          Índice del segundo elemento de la serie
   *
   * @param C
   *          Índice del tercer elemento de la serie
   *
   * @param partida
   *          Objeto con la información del juego
   *
   * @return
   *          'true' si los elementos estaban en la misma fila; o 'false' si no
   */
  def misma_fila (A: Int, B: Int, C: Int, partida: Partida): Boolean = {

    val cols = partida.columnas

    (
      ((A + 1) == B)
      && ((B + 1) == C)
      && ((A % cols) < (B % cols))
      && ((B % cols) < (C % cols))
    )
  }

  /**
   * Comprueba si dos elementos están en la misma columna y de manera consecutiva
   *
   * @param A
   *          Índice del primer elemento de la serie
   *
   * @param B
   *          Índice del segundo elemento de la serie
   *
   * @param C
   *          Índice del tercer elemento de la serie
   *
   * @param partida
   *          Objeto con la información del juego
   *
   * @return
   *          'true' si los elementos estaban en la misma columna; o 'false' si no
   */
  def misma_columna (A: Int, B: Int, C: Int, partida: Partida): Boolean = {

    val cols = partida.columnas

    (
      ((A + cols) == B)
      && ((B + cols) == C)
    )
  }

  /**
   * Obtiene la columna especificada de la matriz pasada como argumento.
   *
   * @param partida
   *          Objeto con la información del juego
   *
   * @param lista
   *          Matriz de juego
   *
   * @param col
   *          Columna que se desea obtener (el índice empieza en 0)
   *
   *
   * @return
   *          Una lista con los elementos de la columna especificada
   */
  def obtener_col (partida: Partida, lista: List [Any], col: Int): List [Any] = {

    Utils.mapear_indexado (lista) {
      (e, i) => if (i % partida.columnas == col) e else null
    }
  }

  /**
   * Inserta la lista con la columna en la matriz.
   *
   * @param partida
   *          Objeto con la información del juego
   *
   * @param lista
   *          Matriz de juego
   *
   * @param col
   *          Índice de la columna en la que se deben introducir los datos
   *
   * @param columna
   *          Lista con los datos a introducir
   *
   * @param contador
   *            Contador para saber el índice de la posición actual. Si se quiere empezar
   *          en un índice diferente a 0, se puede establecer el valor deseado.
   *          Si se omite, toma el valor 0 por defecto.
   *
   * @return
   *          Una nueva lista resultado de añadir los elementos de la columna en el
   *        índice indicado a la lista inicial.
   */
  def insertar_col (partida: Partida
                    , lista: List [Any]
                    , col_idx: Int
                    , columna: List [Any]
                    , contador: Int = 0): List [Any] = {

    if (lista.length == 0) {

      Nil
    } else {

      if (contador % partida.columnas == col_idx) {

        columna.head::insertar_col (partida
                                    , lista.tail
                                    , col_idx
                                    , columna.tail
                                    , contador + 1)
      } else {

        lista.head::insertar_col (partida
                                  , lista.tail
                                  , col_idx
                                  , columna
                                  , contador + 1)
      }
    }
  }

  /**
   * Cambia el primer elemento por el segundo en la lista especificada.
   *
   * @param lista
   *          Lista en la que se van a intercambiar los elementos
   *
   * @param pos_1
   *          Primera posición a intercambiar (fila, columna)
   *
   * @param pos_2
   *          Segunda posición a intercambiar (fila, columna)
   *
   *
   * @return
   *          Lista con los elementos intercambiados.
   */
  def cambiar (lista: List [Any], pos_1: Int, pos_2: Int): List [Any] = {

    val elem_1 = lista (pos_1)
    val elem_2 = lista (pos_2)

    Utils.mapear_indexado (lista) {

      case (e, i) => if (i == pos_1) { elem_2 } else { if (i == pos_2) elem_1 else e }
    }
  }


  /**
   * Realiza la operación designada en el archivo pasado como argumento. Por ejemplo,
   * para imprimir los datos del array:
   *
   *    val data = Array ("Pruebas","de","texto")
   *    toFile (new File ("ejemplo.txt")) { p => data.foreach (p.println) }
   *
   *
   * @param archivo
   *          Elemento de tipo java.io.File
   */
  def toFile (archivo: java.io.File)(op: java.io.PrintWriter => Unit) {

    val p = new java.io.PrintWriter (archivo)
    try { op (p) } finally { p.close() }
  }

  /**
   * Carga los datos de una partida que estén guardados en el fichero especificado.
   *
   * @param archivo
   *          Nombre del archivo que se desea cargar.
   *
   *
   * @return
   *          Una tupla con la información de la partida (en un objeto de tipo Partida),
   *        los puntos y la matriz de juego (en ese orden).
   */
  def cargar_partida (archivo: String): (Partida, Int, List [Any]) = {

    if (new java.io.File (archivo).exists ()) {

      val datos = scala.io.Source.fromFile (archivo)
      val lista = try datos.getLines.toList finally datos.close ()

      /* Organización del archivo (con sus índices en la lista):
       *  0 -> Filas
       *  1 -> Columnas
       *  2 -> Nivel
       *  3 -> Puntuación
       *  4 -> Matriz de juego
       */
      val filas: Int = lista (0).filter (c => Character.isDigit (c)).toInt
      val columnas: Int = lista (1).filter (c => Character.isDigit (c)).toInt

      val nivel: Int = lista (2).filter (c => Character.isDigit (c)).toInt
      val puntos: Int = lista (3).filter (c => Character.isDigit (c)).toInt

      val matriz: List [Any] = mapear (lista (4).toList) {

        e => if (e == ' ') null else e
      }
      val matriz_enteros = mapear (matriz) {case e: Char => e.asDigit}

      (new Partida (filas, columnas, nivel), puntos, matriz_enteros)
    } else {

      (null, null.asInstanceOf [Int], null)
    }
  }

  /**
   * Implementación propia de map. Aplica la operación "op" a cada elemento de la lista.
   *
   * @param lista
   *          Lista a la que se le debe aplicar la operación.
   *
   * @param op
   *          Operación a aplicar. Debe tomar como argumento un elemento del mismo tipo
   *        que el de la lista.
   *
   *
   * @return
   *          La lista resultante de aplicar la operación a la lista anterior.
   */
  def mapear [A, B] (lista: List [A])(op: A => B): List [B] = {

    if (lista.length == 0) {

      Nil
    } else {

      val res = op (lista.head)

      if (res != null) res::mapear (lista.tail) {op} else mapear (lista.tail) {op}
    }
  }


  /**
   * Implementación propia de map, pero usando índices. Aplica la operación "op"
   * a cada elemento de la lista.
   *
   * @param lista
   *            Lista a la que se le debe aplicar la operación.
   *
   * @param contador
   *            Contador para saber el índice de la posición actual. Si se quiere empezar
   *          en un índice diferente a 0, se puede establecer el valor deseado.
   *          Si se omite, toma el valor 0 por defecto.
   *
   * @param op
   *          Operación a aplicar. Debe tomar como argumentos un elemento del mismo tipo
   *        que el de la lista y un entero (el contador), en ese orden.
   *
   *
   * @return
   *          La lista resultante de aplicar la operación a la lista anterior.
   */
  def mapear_indexado [A, B] (lista: List [A], contador: Int = 0)
                             (op: (A, Int) => B): List [B] = {

    if (lista.length == 0) {

      Nil
    } else {

      val res = op (lista.head, contador)

      if (res != null) {

        res::mapear_indexado (lista.tail, contador + 1) {op}
      } else {

        mapear_indexado (lista.tail, contador + 1) {op}
      }
    }

  }

  /**
   * Invierte el contenido de la lista.
   *
   * @param lista
   *          Lista a invertir
   *
   *
   * @return
   *          Una nueva lista con los elementos invertidos
   */
  def invertir (lista: List [Any]): List [Any] = {

    def inv_aux (invertida: List [Any], lista: List [Any]) : List[Any] = {

      lista match {

        case Nil => invertida
        case (h::t) => inv_aux (h::invertida, t)
      }
    }

    inv_aux (Nil, lista)
  }

}
