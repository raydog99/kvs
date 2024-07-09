from typing import Dict, Optional
from metadata import Column

class KVStore:
    def __init__(self):
        self.store: Dict[str, bytes] = {}

    def put(self, key: str, value: bytes):
        self.store[key] = value

    def get(self, key: str) -> Optional[bytes]:
        return self.store.get(key)

    def delete(self, key: str):
        if key in self.store:
            del self.store[key]

    @staticmethod
    def generate_column_key(column: Column) -> str:
        return f"{column.table_id}:{column.id}:{column.name}"