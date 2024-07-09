package legostore

import (
	"github.com/raydog99/kvs/legostore/kvstore"
	"github.com/raydog99/kvs/legostore/metadata"
)

type ColumnChunk struct {
	ColumnID       uint32
	DeviceID       uint32
	Size           uint64
	CompressedSize uint64
	Compression    metadata.CompressionAlgorithm
}

type DataRetriever struct {
	ColumnChunks map[uint32][]ColumnChunk
	Buffer       [][]byte
	BufferSize   uint64
}

func NewDataRetriever() *DataRetriever {
	return &DataRetriever{
		ColumnChunks: make(map[uint32][]ColumnChunk),
		Buffer:       [][]byte{},
		BufferSize:   0,
	}
}

func (dr *DataRetriever) CompressColumnChunk(chunk ColumnChunk, data []byte) []byte {
	return data
}

func (dr *DataRetriever) DecompressColumnChunk(chunk ColumnChunk, data []byte) []byte {
	return data
}

func (dr *DataRetriever) RetrieveColumnChunk(chunk ColumnChunk, m *metadata.Metadata, kv *kvstore.KVStore) {
	if column, ok := m.GetColumn(chunk.ColumnID); ok {
		key := kvstore.GenerateColumnKey(column)
		if compressedData, ok := kv.Get(key); ok {
			decompressedData := dr.DecompressColumnChunk(chunk, compressedData)
			dr.Buffer = append(dr.Buffer, decompressedData)
			dr.BufferSize += uint64(len(decompressedData))
		}
	}
}

func (dr *DataRetriever) FetchTableScan(tableID uint32, columnIDs []uint32, m *metadata.Metadata, kv *kvstore.KVStore) {
	for _, colID := range columnIDs {
		if chunks, ok := dr.ColumnChunks[colID]; ok {
			for _, chunk := range chunks {
				dr.RetrieveColumnChunk(chunk, m, kv)
			}
		}
	}
}

func (dr *DataRetriever) GetBufferData() [][]byte {
	data := dr.Buffer
	dr.Buffer = [][]byte{}
	dr.BufferSize = 0
	return data
}

func (dr *DataRetriever) MoveColumnChunk(chunk ColumnChunk, targetDeviceID uint32, m *metadata.Metadata, kv *kvstore.KVStore) {
	if column, ok := m.GetColumn(chunk.ColumnID); ok {
		key := kvstore.GenerateColumnKey(column)
		if compressedData, ok := kv.Get(key); ok {
			decompressedData := dr.DecompressColumnChunk(chunk, compressedData)

			targetDevice, _ := m.GetDevice(targetDeviceID)
			newChunk := ColumnChunk{
				ColumnID:       chunk.ColumnID,
				DeviceID:       targetDeviceID,
				Size:           chunk.Size,
				CompressedSize: 0,
				Compression:    targetDevice.Compression,
			}

			newCompressedData := dr.CompressColumnChunk(newChunk, decompressedData)
			newChunk.CompressedSize = uint64(len(newCompressedData))

			kv.Put(key, newCompressedData)

			if chunks, ok := dr.ColumnChunks[chunk.ColumnID]; ok {
				for i, c := range chunks {
					if c.DeviceID == chunk.DeviceID {
						chunks[i] = newChunk
						break
					}
				}
			}
		}
	}
}

func OptimizePlacement(budget float32, m *metadata.Metadata) []Placement {
	placements := PlaceData(LOPT, m, &budget)
	totalTime := PredictTotalTime(placements, m)
	totalCost := CalculateTotalCost(placements, m)
	println("Predicted total time:", totalTime, "s")
	println("Total cost:", totalCost)
	return placements
}

func ApplyPlacements(placements []Placement, m *metadata.Metadata, kv *kvstore.KVStore, dr *dataretriever.DataRetriever) {
	for _, placement := range placements {
		if column, ok := m.GetColumn(placement.ColumnID); ok {
			if targetDevice, ok := m.GetDevice(placement.DeviceID); ok {
				if chunks, ok := dr.ColumnChunks[placement.ColumnID]; ok {
					for _, chunk := range chunks {
						if chunk.DeviceID != placement.DeviceID {
							dr.MoveColumnChunk(chunk, placement.DeviceID, m, kv)
							println("Moved column", column.Name, "to device", targetDevice.Name)
						}
					}
				}
			}
		}
	}
}