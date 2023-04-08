import util.Pixel
import util.Util.*

import scala.annotation.tailrec

//import java.awt.Color

// Online viewer: https://0xc0de.fr/webppm/
object Solution {
  type Image = List[List[Pixel]]
  type GrayscaleImage = List[List[Double]]

  // prerequisites
  def fromStringPPM(image: List[Char]): Image = {
    def split(delim: Char)(s: List[Char]): List[List[Char]] = {

      //splits List of char by op
      def op(c: Char, acc: List[List[Char]]): List[List[Char]] =
        acc match {
          case Nil => if (c == delim) Nil else List(List(c))
          case x :: xs => if (c == delim) Nil :: acc else (c :: x) :: xs
        }

      s.foldRight(Nil: List[List[Char]])(op)
    }

    //makes List[List[Int]] from List[Char] and takes first element of second row (nr Columns of matrix)
    val nrColumns = split('\n')(image).drop(1).
      map(split(' ')).
      map(_.map(_.mkString.toInt).head).head

    //makes List[List[Pixel]]  with number of Pixels corresponding to number of columns
    split('\n')(image).drop(3).map(split(' ')).
      flatMap(_.map(_.mkString.toInt)).
      grouped(3).map(x => Pixel(x(0), x(1), x(2))).toList.grouped(nrColumns).
      map(_.toList).toList
  }

  def toStringPPM(image: Image): List[Char] = {

    //creates list header
    val header = "P3\n" + image.head.length.toString + " " + image.length.toString + "\n" + "255" + "\n"

    // converts image to List[Char]
    val imageToList = image.map(_.map(x => x.red.toString + " " + x.green.toString + " " + x.blue.toString).
      mkString("\n")).mkString("\n").toList

    //combines header and converted image
    header.toList ++ imageToList ++ List('\n')
  }
  // ex 1
  def verticalConcat(image1: Image, image2: Image): Image = {
    image1 ++ image2
  }
  // ex 2
  def horizontalConcat(image1: Image, image2: Image): Image = {
    (image1 zip image2).map((x,y) => x ++ y)
  }

  // ex 3
  def rotate(image: Image, degrees: Integer): Image = {
    //computes number of 90 degree rotations
    val nrRotations = (degrees/90).toInt

    //reverse a matrix
    def reverse(l: List[List[Pixel]]): List[List[Pixel]] = {
      def loop(acc: List[List[Pixel]], l: List[List[Pixel]]): List[List[Pixel]] =
        l match {
          case Nil => acc
          case x :: xs => loop(x :: acc, xs)
        }

      loop(Nil, l)
    }

    //transpose pixel matrix
    def transpose(m: List[List[Pixel]]): List[List[Pixel]] =
      m match {
        case Nil :: _ => Nil
        case _ => m.map(_.head) :: transpose(m.map(_.tail))
      }

    //rotates the image by applying transpose and reverse
    @tailrec
    def rotate(nrRotations: Int, image: Image): Image ={
      if (nrRotations == 0) image
      else rotate(nrRotations - 1, reverse(transpose(image)))
    }

    rotate(nrRotations, image)
  }

  // ex 4
  val gaussianBlurKernel: GrayscaleImage = List[List[Double]](
    List( 1, 4, 7, 4, 1),
    List( 4,16,26,16, 4),
    List( 7,26,41,26, 7),
    List( 4,16,26,16, 4),
    List( 1, 4, 7, 4, 1)
  ).map(_.map(_ / 273))

  val Gx : GrayscaleImage = List(
    List(-1, 0, 1),
    List(-2, 0, 2),
    List(-1, 0, 1)
  )

  val Gy : GrayscaleImage = List(
    List( 1, 2, 1),
    List( 0, 0, 0),
    List(-1,-2,-1)
  )

  def edgeDetection(image: Image, threshold : Double): Image = {
    //converts image to grayscale image
    val grayscale = image.map(_.map(x => toGrayScale(x)))

    //applies gaussian blur using convolution
    val gaussianBlur = applyConvolution(grayscale, gaussianBlurKernel)

    //generates matrices of sharp intensity variation detection by applying Gx and Gy kernels using convolution
    val Mx = applyConvolution(gaussianBlur, Gx)
    val My = applyConvolution(gaussianBlur, Gy)

    //combines Mx and My matrices by summing each element, constructs image taking in consideration color threshold
    (Mx zip My).map((x,y) => (x zip y).
      map((a, b) => if((a.abs + b.abs) >= threshold) Pixel(255,255,255) else Pixel(0,0,0)))
  }

  //multiplies pixel neighbours with kernel and sums the result to form convoluted matrix
  def applyConvolution(image: GrayscaleImage, kernel: GrayscaleImage) : GrayscaleImage = {
    getNeighbors(image, (kernel.length/2).toInt).
      map(_.map(x => (x zip kernel).map((x,y) => (x zip y).map((a,b) => (a*b)).sum).sum).toList)
  }

  // ex 5
  def moduloPascal(m: Integer, funct: Integer => Pixel, size: Integer): Image = {

    //takes as input last generated row, next next row of pascal triangle
    def makeRow(l: List[Int]): List[Int] = {
      l match 
        case Nil => Nil
        case x :: Nil => List(x) ++ List(x)
        case x :: xs => List(1) ++ l.sliding(2).map(_.sum).toList.map(x=>x%m) ++ List(1)
    }

    //populates pascal with size number of rows of pascal triangle
    @tailrec
    def makePascal(pascal: List[List[Int]], counter: Int): List[List[Int]] = {
      if (counter == size) pascal
      else {
        makePascal(pascal ++ List(makeRow(pascal.last)), counter + 1)
      }
    }



    //fills the rest of the image with black pixels, converts List[List[Int]] to Image
    makePascal(List(List(1)), 1).
      map(x=>x++List.fill(size-x.length)(4)).
      map(_.map(x => funct(x)))
  }
}