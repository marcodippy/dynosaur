/*
 * Copyright 2019 OVO Energy
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package avro

trait AvroSerialiser {
  def serialise(avroType: AvroType): Array[Byte]
}

object AvroSerialiser {
  def create: AvroSerialiser = new AvroSerialiser {
    import java.io.ByteArrayOutputStream

    import org.apache.avro.generic.{GenericDatumWriter, GenericRecord}
    import org.apache.avro.io.EncoderFactory

    override def serialise(avroType: AvroType): Array[Byte] = {

      val outputStream = new ByteArrayOutputStream()
      val encoder = EncoderFactory.get.binaryEncoder(outputStream, null)

      avroType match {
        case AvroInt(i) => encoder.writeInt(i)
        case AvroString(s) => encoder.writeString(s)
        case ar: AvroRecord => {
          val user = ar.toRawAvro
          val datumWriter = new GenericDatumWriter[GenericRecord](ar.schema.toRawAvro)
          val record: org.apache.avro.generic.GenericData.Record =
            user.asInstanceOf[org.apache.avro.generic.GenericData.Record]

          datumWriter.write(record, encoder)
        }
      }

      encoder.flush()

      outputStream.toByteArray
    }
  }

}
