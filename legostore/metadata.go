package legostore

import (
	"fmt"
)

type CompressionAlgorithm int

const (
	None CompressionAlgorithm = iota
	LZ4
	ZSTD
)

type Device struct {
	ID           uint32
	Mnt          string
	Name         string
	Capacity     uint64
	Threads      uint32
	Compression  CompressionAlgorithm
	CostPerGB    float32
}

type Column struct {
	ID        uint32
	TableID   uint32
	Name      string
	DataType  string
	Size      uint64
}

type Table struct {
	ID      uint32
	Name    string
	Columns []Column
}

type TraceEntry struct {
	TableID   uint32
	ColumnIDs []uint32
}

type DeviceModel struct {
	DeviceID        uint32
	NoneThroughput  float32
	LZ4Throughput   float32
	ZSTDThroughput  float32
	SeekTime        float32
}

type Metadata struct {
	Devices      map[uint32]Device
	Tables       map[uint32]Table
	Columns      map[uint32]Column
	Trace        []TraceEntry
	DeviceModels map[uint32]DeviceModel
}

func NewMetadata() *Metadata {
	return &Metadata{
		Devices:      make(map[uint32]Device),
		Tables:       make(map[uint32]Table),
		Columns:      make(map[uint32]Column),
		Trace:        []TraceEntry{},
		DeviceModels: make(map[uint32]DeviceModel),
	}
}

func (m *Metadata) AddDevice(device Device) {
	m.Devices[device.ID] = device
}

func (m *Metadata) GetDevice(deviceID uint32) (Device, bool) {
	device, ok := m.Devices[deviceID]
	return device, ok
}

func (m *Metadata) AddTable(table Table) {
	m.Tables[table.ID] = table
	for _, column := range table.Columns {
		m.Columns[column.ID] = column
	}
}

func (m *Metadata) GetTable(tableID uint32) (Table, bool) {
	table, ok := m.Tables[tableID]
	return table, ok
}

func (m *Metadata) GetColumn(columnID uint32) (Column, bool) {
	column, ok := m.Columns[columnID]
	return column, ok
}

func (m *Metadata) AddTraceEntry(entry TraceEntry) {
	m.Trace = append(m.Trace, entry)
}

func (m *Metadata) ClearTrace() {
	m.Trace = []TraceEntry{}
}

func (m *Metadata) LoadDeviceModel(model DeviceModel) {
	m.DeviceModels[model.DeviceID] = model
}

func (m *Metadata) GetDeviceModel(deviceID uint32) (DeviceModel, bool) {
	model, ok := m.DeviceModels[deviceID]
	return model, ok
}