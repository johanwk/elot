CREATE TABLE IF NOT EXISTS schema_version (
  version INTEGER PRIMARY KEY
);

CREATE TABLE IF NOT EXISTS sources (
  source        TEXT NOT NULL,
  data_source   TEXT NOT NULL DEFAULT '',
  type          TEXT,
  last_modified REAL,
  last_updated  REAL,
  PRIMARY KEY (source, data_source)
);

CREATE TABLE IF NOT EXISTS entities (
  id          TEXT,
  label       TEXT,
  source      TEXT NOT NULL,
  data_source TEXT NOT NULL DEFAULT '',
  kind        TEXT NOT NULL DEFAULT 'unknown',
  PRIMARY KEY (id, source, data_source),
  FOREIGN KEY (source, data_source)
    REFERENCES sources(source, data_source) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS attributes (
  id          TEXT,
  source      TEXT NOT NULL,
  data_source TEXT NOT NULL DEFAULT '',
  prop        TEXT,
  value       TEXT,
  lang        TEXT NOT NULL DEFAULT '',
  FOREIGN KEY (id, source, data_source)
    REFERENCES entities(id, source, data_source) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS prefixes (
  source        TEXT NOT NULL,
  data_source   TEXT NOT NULL DEFAULT '',
  prefix        TEXT NOT NULL,
  expansion     TEXT NOT NULL,
  PRIMARY KEY (source, data_source, prefix),
  FOREIGN KEY (source, data_source)
    REFERENCES sources(source, data_source) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS global_prefixes (
  prefix      TEXT PRIMARY KEY,
  expansion   TEXT NOT NULL,
  trust       INTEGER NOT NULL DEFAULT 1
);

CREATE INDEX IF NOT EXISTS idx_attrs_id_source
  ON attributes(id, source, data_source);
CREATE INDEX IF NOT EXISTS idx_entities_id
  ON entities(id);
CREATE INDEX IF NOT EXISTS idx_prefixes_expansion
  ON prefixes(expansion);
CREATE INDEX IF NOT EXISTS idx_attrs_prop_lang
  ON attributes(prop, lang);
