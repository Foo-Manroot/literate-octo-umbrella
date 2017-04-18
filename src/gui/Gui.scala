/* ---------------------------------------------- */
/* Funciones relacionadas con la interfaz gráfica */
/* ---------------------------------------------- */

import scala.swing._

class GUI extends MainFrame {

  /* Establece los parámetros de la ventana */
  title = "Candy"
  contents = new Label ("Contenido")
  preferredSize = new Dimension (320, 240)

  /* Centra la ventana en la pantalla */
  peer.setLocationRelativeTo (null)
}


object GUI_main {

  def main (args: Array[String]) {

    val ui = new GUI
    ui.visible = true
    println (" ---- FIN ---- ")
  }
}
