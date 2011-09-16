package sbinary

import Operations._
import collection.{ mutable, immutable, Seq }
import collection.generic.CanBuildFrom
import java.io.File
import java.net.{ URI, URL }
import scala.xml.{ XML, Elem, NodeSeq }
import scala.reflect.ClassManifest

trait BasicTypes extends CoreProtocol {
  implicit def optionsAreFormat[S: Format] : Format[Option[S]] = new Format[Option[S]] {
    def reads(in : Input) =
      if (read[Byte](in) == 0) None
      else Some(read[S](in))

    def writes(out : Output, s: Option[S]) = s match {
      case Some(x) => { write[Byte](out, 1); write(out, x) }
      case None    => write[Byte](out, 0)
    }
  }

<#list 2..22 as i>
  <#assign typeName>
   Tuple${i}[<#list 1..i as j>T${j} <#if i != j>,</#if></#list>]
  </#assign>
  implicit def tuple${i}Format[<#list 1..i as j><#if i == 2>@specialized </#if>T${j}<#if i !=j>,</#if></#list>](implicit
    <#list 1..i as j>
      bin${j} : Format[T${j}] <#if i != j>,</#if>
    </#list>
    ) : Format[${typeName}] = new Format[${typeName}]{
      def reads (in : Input) : ${typeName} = (
    <#list 1..i as j>
        read[T${j}](in)<#if i!=j>,</#if>
    </#list>
      )

      def writes(out : Output, tuple : ${typeName}) = {
      <#list 1..i as j>
        write(out, tuple._${j})
      </#list>
      }
  }
</#list>
}

trait CollectionTypes extends BasicTypes with Generic {
  private[this] val arrayFormatCache = mutable.HashMap[ClassManifest[_], Format[Array[_]]]()

  implicit def listFormat[T: Format] : Format[List[T]] = new LengthEncoded[List, T]

  /**
   * Format instance which encodes the collection by first writing the length
   * of the collection as an int, then writing the collection elements in order.
   */
  final class LengthEncoded[CC[X] <: Traversable[X], T](implicit binT : Format[T], cbf: CanBuildFrom[Nothing, T, CC[T]])
        extends Format[CC[T]] {
    def reads(in: Input): CC[T] = {
      val size = read[Int](in)
      val builder = cbf()
      builder sizeHint size
      var i = 0
      while (i < size) {
        builder += read[T](in)
        i += 1
      }
      builder.result()
    }
    def writes(out: Output, ts: CC[T]) {
      val len = ts.size
      write(out, len)
      ts foreach (t => write(out, t))
    }
  }

  final class ArrayFormat[T : ClassManifest : Format] extends Format[Array[T]] {
    def reads(in: Input) = {
      val size = read[Int](in)
      val result = new Array[T](size)
      var i = 0
      while (i < size) {
        result(i) = read[T](in)
        i += 1
      }
      result
    }
    def writes(out: Output, ts: Array[T]) = {
      val len = ts.length
      write(out, len)
      var i = 0
      while (i < len) {
        write(out, ts(i))
        i += 1
      }
    }
  }

  implicit def arrayFormat[T](implicit fmt: Format[T], mf: ClassManifest[T]): Format[Array[T]] = (
    if (mf == ClassManifest.Byte)
      ByteArrayFormat
    else if (arrayFormatCache contains mf)
      arrayFormatCache(mf)
    else {
      val newFormat = new ArrayFormat[T]
      arrayFormatCache(mf) = newFormat.asInstanceOf[Format[Array[_]]]
      newFormat
    }
  ).asInstanceOf[Format[Array[T]]]
  
  implicit object ByteArrayFormat extends Format[Array[Byte]] {
    def reads(in : Input) = {
      val length = read[Int](in)
      val bytes  = new Array[Byte](length)
      in readFully bytes
      bytes
    }

    def writes(out : Output, bytes : Array[Byte]) {
      write(out, bytes.length)
      out writeAll bytes
    }
  }

  implicit def mutableSetFormat[T: Format] : Format[mutable.Set[T]] =
    viaSeq((x : Seq[T]) => mutable.Set(x :_*))

  implicit def immutableSetFormat[T: Format] : Format[immutable.Set[T]] =
    viaSeq((x : Seq[T]) => immutable.Set(x :_*))

  implicit def immutableSortedSetFormat[S : Ordering : Format] : Format[immutable.SortedSet[S]] =
    viaSeq( (x : Seq[S]) => immutable.TreeSet[S](x :_*))

  implicit def immutableMapFormat[S: Format, T: Format] : Format[immutable.Map[S, T]] =
    viaSeq( (x : Seq[(S, T)]) => immutable.Map(x :_*))

  implicit def immutableSortedMapFormat[S : Ordering : Format, T: Format] : Format[immutable.SortedMap[S, T]] =
    viaSeq( (x : Seq[(S, T)]) => immutable.TreeMap[S, T](x :_*))

  /**
   * Format instance for streams.
   * Note that unlike almost all other collections this is not length encoded
   * Instead it is encoded with a sequence of byte separators, with a single
   * byte value of 1 preceding each element to be read and a value of 0 indicating
   * the stream termination.
   *
   * This is to ensure proper laziness behaviour - values will be written as they
   * become available rather than thunking the entire stream up front.
   *
   * Warning! The resulting Stream is not read lazily. If you wish to read a Stream
   * lazily you may consider it to be a sequence of Option[T]s terminated by a None.
   *
   * Note that this behaviour has changed from that of SFormat 0.2.1, though the format
   * remains the same.
   */
  implicit def streamFormat[S](implicit bin : Format[S]) : Format[Stream[S]] = new Format[Stream[S]] {
    def reads(in : Input) = {
      val buffer = new mutable.ArrayBuffer[S]
      while((read[Option[S]](in) match {
        case Some(s) => buffer += s; true
        case None => false
      })){}
      buffer.toStream
    }

    def writes(out : Output, stream : Stream[S]) {
      stream.foreach(x => { write[Byte](out, 1); write(out, x); })
      write[Byte](out, 0)
    }
  }
}

trait StandardTypes extends CollectionTypes {
  implicit object BigIntFormat extends Format[BigInt]{
    def reads(in : Input) = BigInt(read[Array[Byte]](in))
    def writes(out : Output, i : BigInt) = write(out, i.toByteArray)
  }

  implicit object BigDecimalFormat extends Format[BigDecimal]{
    def reads(in : Input) = BigDecimal(read[String](in))
    def writes(out : Output, d : BigDecimal) = write(out, d.toString)
  }

  implicit object ClassFormat extends Format[Class[_]]{
    def reads(in : Input) = Class.forName(read[String](in))
    def writes(out : Output, clazz : Class[_]) = write(out, clazz.getName)
  }

  implicit lazy val SymbolFormat : Format[Symbol] = viaString(Symbol(_))
  implicit lazy val FileFormat : Format[File]     = viaString(new File(_ : String))
  implicit lazy val UrlFormat : Format[URL]       = viaString(new URL(_ : String))
  implicit lazy val UriFormat : Format[URI]       = viaString(new URI(_ : String))
  implicit lazy val XmlFormat : Format[NodeSeq]   = new Format[NodeSeq] {
    def reads(in : Input) = XML.loadString(read[String](in)).child
    def writes(out : Output, elem: NodeSeq) = write(out, <binary>elem</binary>.toString)
  }
}
