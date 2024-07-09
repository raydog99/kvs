from dataclasses import dataclass
from enum import Enum
from typing import List, Dict

class CompressionAlgorithm(Enum):
    NONE = 0
    LZ4 = 1
    ZSTD = 2

@dataclass
class Device:
    id: int
    mnt: str
    name: str
    capacity: int
    threads: int
    compression: CompressionAlgorithm
    cost_per_gb: float

@dataclass
class Column:
    id: int
    table_id: int
    name: str
    data_type: str
    size: int

@dataclass
class Table:
    id: int
    name: str
    columns: List[Column]

@dataclass
class TraceEntry:
    table_id: int
    column_ids: List[int]

@dataclass
class DeviceModel:
    device_id: int
    none_throughput: float
    lz4_throughput: float
    zstd_throughput: float
    seek_time: float

class Metadata:
    def __init__(self):
        self.devices: Dict[int, Device] = {}
        self.tables: Dict[int, Table] = {}
        self.columns: Dict[int, Column] = {}
        self.trace: List[TraceEntry] = []
        self.device_models: Dict[int, DeviceModel] = {}

    def add_device(self, device: Device):
        self.devices[device.id] = device

    def get_device(self, device_id: int) -> Device:
        return self.devices.get(device_id)

    def add_table(self, table: Table):
        self.tables[table.id] = table
        for column in table.columns:
            self.columns[column.id] = column

    def get_table(self, table_id: int) -> Table:
        return self.tables.get(table_id)

    def get_column(self, column_id: int) -> Column:
        return self.columns.get(column_id)

    def add_trace_entry(self, entry: TraceEntry):
        self.trace.append(entry)

    def clear_trace(self):
        self.trace.clear()

    def load_device_model(self, model: DeviceModel):
        self.device_models[model.device_id] = model

    def get_device_model(self, device_id: int) -> DeviceModel:
        return self.device_models.get(device_id)