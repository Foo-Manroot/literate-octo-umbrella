package interfaz

import javax.swing._   
import java.awt.event._
import JFrame._ 

import partida._

class Interfaz extends JFrame("Jewels Legend Hero") { 
  
  val azul:ImageIcon = new ImageIcon("./img/azul.png")    
  val rojo:ImageIcon = new ImageIcon("./img/rojo.png") 
  val naranja:ImageIcon = new ImageIcon("./img/naranja.png") 
  val verde:ImageIcon = new ImageIcon("./img/verde.png") 
  val plata:ImageIcon = new ImageIcon("./img/plata.png") 
  val morado:ImageIcon = new ImageIcon("./img/morado.png")
  val gris:ImageIcon = new ImageIcon("./img/gris.png") 
  val amarillo:ImageIcon = new ImageIcon("./img/amarillo.png") 
  val vacio:ImageIcon = new ImageIcon("./img/vacio.png") 
   
  def crear_ventana(partida: Partida, estado: (Int, List [Any])): Unit = {
    
    //tamaño de diamante 50 * 50
    val filas = partida.filas
    val columnas = partida.columnas
    
    
    
    setDefaultLookAndFeelDecorated(true) 

    setDefaultCloseOperation(EXIT_ON_CLOSE) 

    /*val button = new JButton()
    //getContentPane() add button

    
    button.setText("Funciona")
   
    button addActionListener new ActionListener { 
    def actionPerformed(e : ActionEvent) = 
        Console println "Hello world" 
    }
   */
    val panel: JPanel = new JPanel()
    panel.setSize(filas * 100, columnas * 100)
    val boton = crear_boton(8)
    
    panel.add(boton)
   
    this.setContentPane(panel)
    //pack /*No se que mierdas hace esto*/
   
    setSize(filas * 100, columnas * 100)
    setVisible(true);
  }

  /*Crea una lista con todos los botones*/
  def crear_lista_botones(estado: (Int, List [Any])): List[JButton] = { 
    
   val boton = crear_boton(estado._2.head)
   
   if(estado._2.length == 1) boton::Nil   
   
   else boton::crear_lista_botones(estado._1,estado._2.tail)
   
   
  
  }
  /*Crea un boton segun el identificador*/
  def crear_boton(id: Any): JButton = {
    val button = new JButton()
    button.setSize(100, 100)
    id match {
      case 1 => button.setIcon(azul);button
      case 2 => button.setIcon(rojo);button
      case 3 => button.setIcon(naranja);button
      case 4 => button.setIcon(verde);button
      case 5 => button.setIcon(plata);button
      case 6 => button.setIcon(morado);button
      case 7 => button.setIcon(gris);button
      case 8 => button.setIcon(amarillo);button
      case _ => button.setIcon(vacio);button
    }
  }
} 
