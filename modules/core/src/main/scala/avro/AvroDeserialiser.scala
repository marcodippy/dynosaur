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

trait AvroDeserialiser {
  def deserialise(bytes: Array[Byte], avroSchema: AvroSchema): AvroType
}

object AvroDeserialiser {
  def create: AvroDeserialiser = new AvroDeserialiser {
    import org.apache.avro.generic.{GenericDatumReader, GenericRecord}
    import org.apache.avro.io.DecoderFactory

    override def deserialise(bytes: Array[Byte], avroSchema: AvroSchema): AvroType = {
      val decoder = DecoderFactory.get.binaryDecoder(bytes, null)

      avroSchema match {
        case AvroIntSchema => AvroInt(decoder.readInt())
        case AvroStringSchema => AvroString(decoder.readString())
        case avs: AvroRecordSchema => {
          val reader = new GenericDatumReader[GenericRecord](avs.toRawAvro)
          val result: GenericRecord = reader.read(null, decoder)

          AvroType.fromRawAvro(result, avroSchema).asInstanceOf[AvroRecord]
        }
      }

    }
  }
}
