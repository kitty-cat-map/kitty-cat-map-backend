
-- Enable the UUID extension.
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE IF NOT EXISTS image_info (
  id UUID CONSTRAINT firstkey PRIMARY KEY DEFAULT uuid_generate_v4(),
  filename TEXT
  );

-- Create the geom column on the image_info table.
SELECT AddGeometryColumn ('image_info','geom',4326,'POINT',2);
