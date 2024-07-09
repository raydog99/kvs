package legostore

import (
	"github.com/raydog99/kvs/legostore/dataretriever"
	"github.com/raydog99/kvs/legostore/kvstore"
	"github.com/raydog99/kvs/legostore/metadata"
	"github.com/raydog99/kvs/legostore/strategies"
	"strconv"
	"strings"
	"time"
)

type Mosaic struct {
	Metadata      *metadata.Metadata
	KVStore       *kvstore.KVStore
	DataRetriever *dataretriever.DataRetriever
}

func NewMosaic() *Mosaic {
	return &Mosaic{
		Metadata:      metadata.NewMetadata(),
		KVStore:       kvstore.NewKVStore(),
		DataRetriever: dataretriever.NewDataRetriever(),
	}
}

func (m *Mosaic) SetupDevices() {
	m.Metadata.AddDevice(metadata.Device{
		ID:          1,
		Mnt:         "/mnt/nvme",
		Name:        "NVMe SSD",
		Capacity:    1000,
		Threads:     8,
		Compression: metadata.ZSTD,
		CostPerGB:   0.25,
	})
	m.Metadata.AddDevice(metadata.Device{
		ID:          2,
		Mnt:         "/mnt/ssd",
		Name:        "SATA SSD",
		Capacity:    2000,
		Threads:     4,
		Compression: metadata.LZ4,
		CostPerGB:   0.15,
	})
	m.Metadata.AddDevice(metadata.Device{
		ID:          3,
		Mnt:         "/mnt/hdd",
		Name:        "HDD",
		Capacity:    4000,
		Threads:     1,
		Compression: metadata.None,
		CostPerGB:   0.05,
	})
}

func (m *Mosaic) SetupTables() {
	customerColumns := []metadata.Column{
		{ID: 1, TableID: 1, Name: "c_custkey", DataType: "INT", Size: 4 * 150000},
		{ID: 2, TableID: 1, Name: "c_name", DataType: "VARCHAR", Size: 25 * 150000},
		{ID: 3, TableID: 1, Name: "c_address", DataType: "VARCHAR", Size: 40 * 150000},
	}
	m.Metadata.AddTable(metadata.Table{ID: 1, Name: "customer", Columns: customerColumns})

	ordersColumns := []metadata.Column{
		{ID: 4, TableID: 2, Name: "o_orderkey", DataType: "INT", Size: 4 * 1500000},
		{ID: 5, TableID: 2, Name: "o_custkey", DataType: "INT", Size: 4 * 1500000},
		{ID: 6, TableID: 2, Name: "o_totalprice", DataType: "DECIMAL", Size: 8 * 1500000},
	}
	m.Metadata.AddTable(metadata.Table{ID: 2, Name: "orders", Columns: ordersColumns})
}

func (m *Mosaic) ParseQuery(queryStr string) metadata.TraceEntry {
	parts := strings.Split(queryStr, ":")
	tableID, _ := strconv.Atoi(parts[0])
	columnIDs := []uint32{}
	for _, colIDStr := range strings.Split(parts[1], ",") {
		colID, _ := strconv.Atoi(colIDStr)
		columnIDs = append(columnIDs, uint32(colID))
	}
	return metadata.TraceEntry{TableID: uint32(tableID), ColumnIDs: columnIDs}
}

func (m *Mosaic) RunQuery(queryStr string) {
	query := m.ParseQuery(queryStr)
	m.DataRetriever.FetchTableScan(query.TableID, query.ColumnIDs, m.Metadata, m.KVStore)
	result := m.DataRetriever.GetBufferData()
	println("Query executed:", queryStr)
	println("Retrieved", len(result), "columns")
}

func (m *Mosaic) OptimizeStorage(budget float32) {
	placements := strategies.OptimizePlacement(budget, m.Metadata)
	strategies.ApplyPlacements(placements, m.Metadata, m.KVStore, m.DataRetriever)
}

func (m *Mosaic) RunMosaic(budget float32, queries []string) {
	println("Initial benchmark...")
	initialTime := m.Benchmark(queries)
	println("Initial execution time:", initialTime, "s")

	println("\nOptimizing storage...")
	m.OptimizeStorage(budget)

	println("\nRunning benchmark after optimization...")
	optimizedTime := m.Benchmark(queries)
	println("Optimized execution time:", optimizedTime, "s")

	improvement := (initialTime - optimizedTime) / initialTime * 100
	println("\nImprovement:", improvement, "%")
}

func (m *Mosaic) Benchmark(queries []string) float64 {
	start := time.Now()
	for _, query := range queries {
		m.RunQuery(query)
	}
	elapsed := time.Since(start)
	return elapsed.Seconds()
}