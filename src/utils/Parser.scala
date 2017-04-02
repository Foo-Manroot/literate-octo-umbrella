/* -------------------------------------- */
/* Funciones para procesar los argumentos */
/* -------------------------------------- */
package utils

import malla.Malla
import scala.util.matching.Regex

object Utils {

  val ayuda = "Uso: \n" +
              ""
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
   *        este parámetro puede ser una función vacía: () => ()
   */
  def if_else [T] (cond: Boolean, cod_if: () => Unit, cod_else: () => Unit): Unit = {

    if ( (cond) ) {

      cod_if ()
    } else {

      cod_else ()
    }
  }


  /**
   * Obtiene el único argumento aceptado (el nivel de juego) y crea la malla de juego.
   *
   * @param args
   *            Array con
   */
  def obtener_args (args: Array [String]): Malla = {

    val patrón = "^\\d+$".r

    if (args.length != 1) {

      println (ayuda)
      sys.exit (1)
    } else {

      if ( esNum (args (0)) ) {

        Factoría.crear_malla (args (0).toInt)
      } else {

          log_error ("El argumento especificado, '"+ args (0) +
                      "' no es un número válido");
          sys.exit (-1)
      }
    }
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
  def esNum (x: String) = x forall Character.isDigit;
}
