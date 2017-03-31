/* -------------------------------------------------------- */
/* Funciones principales del juego                          */
/* -------------------------------------------------------- */
package main

import parser.Parser
import malla.Malla


object Main {

  /**
   * Función inicial.
   *
   * Se encarga de llamar a todas las funciones necesarias para obtener los argumentos
   * e iniciar el bucle principal.
   */
  def main (args: Array[String]) {

    val malla: Malla = Parser.obtener_args

    println ("asdf");
  }
}
