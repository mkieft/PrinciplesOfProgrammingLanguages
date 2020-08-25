package edu.colorado.csci3155.project2

/* A class to maintain a canvas. */

//import java.awt.geom.{Ellipse2D, Rectangle2D}
import java.awt.{Graphics2D}

/* A figure is a sealed trait. It can be a Polygon or a "MyCircle"*/
sealed trait Figure {
    def getBoundingBox: (Double, Double, Double, Double)
    def translate(shiftX: Double, shiftY: Double): Figure
    def rotate(angRad:Double):Figure
    def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double): Unit
}

/*
 Class Polygon
   A polygon is defined by a list of its vertices
 */

case class Polygon(val cList: List[(Double, Double)]) extends Figure {
    //TODO: Define the bounding box of the polygon
    override def getBoundingBox: (Double, Double, Double, Double ) = {
        val xmin = cList.foldLeft(Double.MaxValue){ (acc, i ) => {
            val(x1,_) = i
            if(x1 < acc) x1 else acc
        }}
        val xmax = cList.foldLeft(Double.MinValue){ (acc, i) => {
            val(x1, _) = i
            if(x1 > acc) x1 else acc
        }}
        val ymin = cList.foldLeft(Double.MaxValue) { (acc, i) => {
            val(_, y1) = i
            if(y1 < acc) y1 else acc
        }}
        val ymax = cList.foldLeft(Double.MinValue){ (acc, i) => {
            val(_,y1) = i
            if(y1 > acc) y1 else acc
        }}
        (xmin, xmax, ymin, ymax)
    }
    //TODO: Create a new polygon by shifting each vertex in cList by (x,y)
    //    Do not change the order in which the vertices appear
    override def translate(shiftX: Double, shiftY: Double): Polygon = {
        Polygon(cList.map{ i => (i._1 + shiftX, i._2 + shiftY)})

    }
    override def rotate(angRad:Double): Polygon ={
        new Polygon(cList.map((i)=> {
            val (x,y) = i
            (x*math.cos(angRad) - y* math.sin(angRad), x*math.sin(angRad) + y*math.cos(angRad))
        }))
    }

    // Function: render -- draw the polygon. Do not edit this function.
    override def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double) = {
        val xPoints: Array[Int] = new Array[Int](cList.length)
        val yPoints: Array[Int] = new Array[Int](cList.length)
        for (i <- 0 until cList.length){
            xPoints(i) = ((cList(i)._1 + shiftX )* scaleX).toInt
            yPoints(i) = ((cList(i)._2 + shiftY) * scaleY).toInt
        }
        g.drawPolygon(xPoints, yPoints, cList.length)
    }
}

/*
  Class MyCircle
  Define a circle with a given center c and radius r
 */
case class MyCircle(val c: (Double, Double), val r: Double) extends Figure {
    //TODO: Define the bounding box for the circle
    override def getBoundingBox: (Double, Double, Double, Double) = {
        val (x,y) = c
        (x-r, x+r, y-r, y+r)
    }


    //TODO: Create a new circle by shifting the center
    override def translate(shiftX: Double, shiftY: Double): MyCircle = {
        val (x_o, y_o) = c
        MyCircle((x_o + shiftX, y_o +shiftY),r)
    }
    override def rotate(angRad:Double): MyCircle ={

        val (x,y) = c
        MyCircle((x*math.cos(angRad) - y * math.sin(angRad), x * math.sin(angRad) + y * math.cos(angRad)), r)
    }
    // Function: render -- draw the polygon. Do not edit this function.
    override def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double) = {
        val centerX = ((c._1 + shiftX) * scaleX) .toInt
        val centerY = ((c._2 + shiftY) * scaleY) .toInt
        val radX = (r * scaleX).toInt
        val radY = (r * math.abs(scaleY)).toInt
        //g.draw(new Ellipse2D.Double(centerX, centerY, radX, radY))
        g.drawOval(centerX-radX, centerY-radY, 2*radX, 2*radY)
    }
}

/*
  Class : MyCanvas
  Define a canvas through a list of figure objects. Figure objects can be circles or polygons.
 */
class MyCanvas (val listOfObjects: List[Figure]) {
    // TODO: Write a function to get the boundingbox for the entire canvas.
    // Hint: use existing boundingbox functions defined in each figure.
    def getBoundingBox: (Double, Double, Double, Double) = {
        val xmin = listOfObjects.foldLeft(Double.MaxValue) { (acc,i) => {
            val (x1,_,_,_) = i.getBoundingBox
            if(x1 < acc) x1 else acc
        }}
        val xmax = listOfObjects.foldLeft(Double.MinValue) { (acc,i) => {
            val(_,x2,_,_) = i.getBoundingBox
            if(x2 > acc) x2 else acc
        }}
        val ymin = listOfObjects.foldLeft(Double.MaxValue) { (acc, i)=> {
          val(_,_,y1,_) = i.getBoundingBox
            if(y1 < acc) y1 else acc
        }}
        val ymax = listOfObjects.foldLeft(Double.MinValue) { (acc, i) => {
            val(_,_,_,y2) = i.getBoundingBox
            if(y2 > acc) y2 else acc
        }}
        (xmin, xmax, ymin, ymax)
    }

    //TODO: Write a function to translate each figure in the canvas by shiftX, shiftY
    def translate(shiftX: Double, shiftY: Double): MyCanvas = {
        new MyCanvas(listOfObjects.map({
            _.translate(shiftX, shiftY)
        }))
    }


    //TODO: Write a function that will return a new MyCanvas object that places
    // all the objects in myc2 to the right of the objects in this MyCanvas.
    // refer to the notebook documentation on how to perform this.
    def placeRight(myc2: MyCanvas):MyCanvas = {

//        val canvasBB = getBoundingBox
//        val (xmin2, xmax2, ymin2, ymax2) = myc2.getBoundingBox
//        new MyCanvas(listOfObjects.map(_.translate(canvasBB._2 - canvasBB._1, (canvasBB._4 - canvasBB._3)/2-(ymax2-ymin2)/2)))
        val (x1min, x1max, y1min, y1max) = getBoundingBox
        val (x2min, x2max, y2min, y2max) = myc2.getBoundingBox
        val xShift = x1max - x1min
        val yShift = (y1max - y1min)/2 - (y2max-y2min)/2
        overlap(myc2 translate (xShift, yShift))
    }

    //TODO: Write a function that will return a new MyCanvas object that places
    // all the figures in myc2 on top of the figures in this MyCanvas.
    // refer to the notebook documentation on how to perform this.
    def placeTop(myc2: MyCanvas): MyCanvas = {
//
        val (x1min, x1max, y1min, y1max) = getBoundingBox
        val (x2min, x2max, y2min, y2max) = myc2.getBoundingBox
        val xShift = (x1max - x1min)/2 - (x2max - x2min)/2
        val yShift = y1max - y1min
        overlap(myc2 translate (xShift, yShift))
    }

    //TODO: Write a function that will rotate each figure in the canvas using
    // the angle `ang` defined in radians.
    // Suggestion: first write rotation functions for polygon and circle.
    // rotating a polygon is simply rotating each vertex.
    // rotating a circle is simply rotating the center with radius unchanged.
    def rotate(angRad: Double): MyCanvas = {
        new MyCanvas(listOfObjects.map((f)=> f.rotate(angRad)))
    }



    // Function to draw the canvas. Do not edit.
    def render(g: Graphics2D, xMax: Double, yMax: Double) = {
        val (lx1, ux1, ly1, uy1) = this.getBoundingBox
        val shiftx = -lx1
        val shifty = -uy1
        val scaleX = xMax/(ux1 - lx1  + 1.0)
        val scaleY = yMax/(uy1 - ly1 + 1.0)
        listOfObjects.foreach(f => f.render(g,scaleX, -scaleY, shiftx, shifty))
    }

    def overlap(c2: MyCanvas): MyCanvas = {
        new MyCanvas(listOfObjects ++ c2.listOfObjects)
    }

    // DO NOT EDIT THE CODE BELOW
    override def toString: String = {
        listOfObjects.foldLeft[String] ("") { case (acc, fig) => acc ++ fig.toString }
    }
    // DO NOT EDIT
    def getListOfObjects: List[Figure] = listOfObjects

    // DO NOT EDIT
    def numPolygons: Int =
        listOfObjects.count {
            case Polygon(_) => true
            case _ => false }

    //DO NOT EDIT
    def numCircles: Int = {
        listOfObjects.count {
            case MyCircle(_,_) => true
            case _ => false }
    }
    //DO NOT EDIT
    def numVerticesTotal: Int = {
        listOfObjects.foldLeft[Int](0) ((acc, f) =>
            f match {
                case Polygon(lst1) => acc + lst1.length
                case _ => acc
            }
        )
    }
}
