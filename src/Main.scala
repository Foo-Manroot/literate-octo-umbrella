/* -------------------------------------------------------- */
/* Funciones principales del juego                          */
/* -------------------------------------------------------- */
package main

import utils._
import malla.Malla


object Main {

  /**
   * Función inicial.
   *
   * Se encarga de llamar a todas las funciones necesarias para obtener los argumentos
   * e iniciar el bucle principal.
   */
  def main (args: Array[String]) {

    val malla: Malla = Utils.obtener_args (args)

    println ("Malla: ");

    malla.imprimir_matriz ();

    println ("asdf");
  }
}
