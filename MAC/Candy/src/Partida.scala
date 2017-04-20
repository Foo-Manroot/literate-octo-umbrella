/* -------------------------------------------------------- */
/* Estructura para almacenar los datos de la malla de juego */
/* -------------------------------------------------------- */
package partida

import utils.Utils
import interfaz._

/**
 * Clase para representar la malla del juego.
 *
 * @param dim_filas
 *        Nº de filas que debe tener la matriz de juego.
 *
 * @param dim_cols
 *        Nº de columnas que debe tener la matriz de juego.
 *
 * @param niv
 *        Nivel del juego
 */
class Partida (dim_filas: Int, dim_cols: Int, niv: Int) {

  val filas: Int = dim_filas
  val columnas: Int = dim_cols
  val nivel: Int = niv

  /**
   * Devuelve una cadena con la información de la partida, incluyendo el estado actual
   * pasado como argumento.
   *
   * @param estado
   *          Tupla con la puntuación y la matriz de juego (en ese orden)
   */
  def toString (estado: (Int, List [Any])): String = {

    "Filas: " + filas + "\n"          +
    "Columnas: " + columnas + "\n"    +
    "Nivel: " + nivel + "\n"          +
    "Puntuación: " + estado._1 + "\n" +
    /* lista.toString devuelve "List(1, 2, 3, ...)"; así que se sustituyen los
     * elementos innecesarios para devolver "1 2 3 4 5..." */
    estado._2.toString.replace (',', ' ')
                      .replace ('L', ' ')
                      .replace ('i', ' ')
                      .replace ('s', ' ')
                      .replace ('t', ' ')
                      .replace ('(', ' ')
                      .replace (')', ' ')
                      .trim
  }

  /**
   *  Imprime el contenido de la matriz, ya formateado. Para imprimir el contenido de
   *  la matriz, sólo hace falta usar esta función tal que malla.imprimir_matriz()
   *
   *  @param lista
   *          Lista a ser impresa, con el número de de columnas especificado en la
   *        declaración del objeto. Puede omitirse (por defecto, es 'matriz').
   *
   *  @param cols
   *          Número de columnas para formatear la matriz. Puede omitirse (por defecto,
   *        es el valor de "columnas")
   */
  def imprimir_matriz (lista: List [Any], cols: Int = columnas): Unit = {

    Utils.mapear_indexado (lista) {
      /* Si se trata de una lista de enteros, imprime los colores. Si no, imprime
      directamente el contenido */
      case (e: Int , i) => if (i % cols == (cols - 1)) {

                  print (diamante (e) + "\n")
                } else {
                  print (diamante (e) + " ")
                }

      case (e, i) => if (i % cols == (cols - 1)) print (e + "\n") else print (e + " ")
    }
  }
  
  /**
   * Dvuelve una cadena para representar el diamante con color, según su valor numérico.
   */
  def diamante (valor: Int): String = {

    valor match {
      case 1 => "A" /*Console.BLUE                  + "1" + Console.RESET*/
      case 2 => "B" /*Console.RED                   + "2" + Console.RESET*/
      case 3 => "C" /*Console.YELLOW                + "3" + Console.RESET*/
      case 4 => "D" /*Console.GREEN                 + "4" + Console.RESET*/
      case 5 => "E" /*Console.CYAN                  + "5" + Console.RESET*/
      case 6 => "F" /*Console.MAGENTA               + "6" + Console.RESET*/
      case 7 => "G" /*Console.BLUE + Console.BOLD   + "7" + Console.RESET*/
      case 8 => "H" /*Console.WHITE                 + "8" + Console.RESET*/
      case _ => "#" /*Console.WHITE + Console.BOLD  + "#" + Console.RESET*/
    }
  }
}