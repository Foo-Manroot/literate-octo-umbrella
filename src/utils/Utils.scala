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
   * Crea una matriz de n enteros aleatorios
   *
   * @param n
   *          Número de elementos a crear
   *
   * @return
   *          Lista con los elementos
   */
  def crear_lista (n: Int): List [Int] = {

    if (n == 0)
      Nil
    else
      (util.Random.nextInt (7) + 1)::crear_matriz (n - 1)
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
}
