package legostore

import (
	"math/rand"
	"github.com/raydog99/kvs/legostore/kvstore"
	"sort"
	"time"
)

type YCSBWorkload struct {
	Name             string
	ReadProportion   float32
	UpdateProportion float32
	InsertProportion float32
}

type YCSBBenchmark struct {
	RecordCount    int
	OperationCount int
	KeyLength      int
	ValueLength    int
}

func NewYCSBBenchmark(recordCount, operationCount int) *YCSBBenchmark {
	return &YCSBBenchmark{
		RecordCount:    recordCount,
		OperationCount: operationCount,
		KeyLength:      10,
		ValueLength:    100,
	}
}

func (y *YCSBBenchmark) generateKey() string {
	const letters = "abcdefghijklmnopqrstuvwxyz"
	b := make([]byte, y.KeyLength)
	for i := range b {
		b[i] = letters[rand.Intn(len(letters))]
	}
	return string(b)
}

func (y *YCSBBenchmark) generateValue() []byte {
	b := make([]byte, y.ValueLength)
	rand.Read(b)
	return b
}

func (y *YCSBBenchmark) LoadPhase(kv *kvstore.KVStore) {
	for i := 0; i < y.RecordCount; i++ {
		key := y.generateKey()
		value := y.generateValue()
		kv.Put(key, value)
	}
}

func (y *YCSBBenchmark) RunWorkload(workload *YCSBWorkload, kv *kvstore.KVStore) (float64, []float64) {
	latencies := make([]float64, y.OperationCount)

	start := time.Now()
	for i := 0; i < y.OperationCount; i++ {
		r := rand.Float32()
		opStart := time.Now()

		if r < workload.ReadProportion {
			key := y.generateKey()
			kv.Get(key)
		} else if r < workload.ReadProportion+workload.UpdateProportion {
			key := y.generateKey()
			value := y.generateValue()
			kv.Put(key, value)
		} else {
			key := y.generateKey()
			value := y.generateValue()
			kv.Put(key, value)
		}

		latencies[i] = time.Since(opStart).Seconds()
	}
	totalTime := time.Since(start).Seconds()

	return totalTime, latencies
}

func (y *YCSBBenchmark) RunBenchmark(kv *kvstore.KVStore) {
	println("Loading data...")
	y.LoadPhase(kv)

	workloads := []YCSBWorkload{
		{"Workload A (50% read, 50% update)", 0.5, 0.5, 0.0},
		{"Workload B (95% read, 5% update)", 0.95, 0.05, 0.0},
		{"Workload C (100% read)", 1.0, 0.0, 0.0},
		{"Workload D (95% read, 5% insert)", 0.95, 0.0, 0.05},
		{"Workload E (95% scan, 5% insert)", 0.95, 0.0, 0.05},
		{"Workload F (50% read, 50% read-modify-write)", 0.5, 0.5, 0.0},
	}

	for _, workload := range workloads {
		println("\nRunning", workload.Name)
		totalTime, latencies := y.RunWorkload(&workload, kv)

		avgLatency := 0.0
		for _, lat := range latencies {
			avgLatency += lat
		}
		avgLatency /= float64(len(latencies))

		throughput := float64(y.OperationCount) / totalTime

		println("Throughput:", throughput, "ops/sec")
		println("Average latency:", avgLatency*1000, "ms")

		sort.Float64s(latencies)
		p95 := latencies[int(float64(y.OperationCount)*0.95)]
		p99 := latencies[int(float64(y.OperationCount)*0.99)]

		println("95th percentile latency:", p95*1000, "ms")
		println("99th percentile latency:", p99*1000, "ms")
	}
}