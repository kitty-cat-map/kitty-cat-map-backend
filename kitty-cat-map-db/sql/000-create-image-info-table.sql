
-- Enable the UUID extension.
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE IF NOT EXISTS image_info (
  id UUID CONSTRAINT firstkey PRIMARY KEY DEFAULT uuid_generate_v4(),
  filename TEXT NOT NULL,
  date TIMESTAMPTZ NOT NULL,
  lat DOUBLE PRECISION NOT NULL CHECK (lat >= -90 AND lat <= 90),
  lon DOUBLE PRECISION NOT NULL CHECK (lon >= -180 AND lon <= 180)
  );

CREATE INDEX image_info_lat_lon_date ON image_info (lat, lon, date DESC);

-- Create the geom column on the image_info table.
SELECT AddGeometryColumn ('image_info','geom',4326,'POINT',2);
