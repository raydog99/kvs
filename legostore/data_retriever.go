package legostore

import (
	"fmt"
	"sync"

	"github.com/raydog99/kvs/legostore/metadata"
)

type KVStore struct {
	store map[string][]byte
	mutex sync.RWMutex
}

func NewKVStore() *KVStore {
	return &KVStore{
		store: make(map[string][]byte),
	}
}

func (kv *KVStore) Put(key string, value []byte) {
	kv.mutex.Lock()
	defer kv.mutex.Unlock()
	kv.store[key] = value
}

func (kv *KVStore) Get(key string) ([]byte, bool) {
	kv.mutex.RLock()
	defer kv.mutex.RUnlock()
	value, ok := kv.store[key]
	return value, ok
}

func (kv *KVStore) Delete(key string) {
	kv.mutex.Lock()
	defer kv.mutex.Unlock()
	delete(kv.store, key)
}

func GenerateColumnKey(column metadata.Column) string {
	return fmt.Sprintf("%d:%d:%s", column.TableID, column.ID, column.Name)
}