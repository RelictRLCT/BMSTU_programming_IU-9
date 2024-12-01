-- Чтоб можно было прогнать скрипт несколько раз
USE master;
GO

IF DB_ID (N'lab') IS NOT NULL
DROP DATABASE lab;
GO

-- 1. Создание базы
CREATE DATABASE lab
	ON (
		NAME = lab_dat, 
		FILENAME = '/home/relict/desktop/db_lab/labdat.mdf', 
		SIZE = 10MB, 
		MAXSIZE = UNLIMITED, 
		FILEGROWTH = 10%
	)
	LOG ON (
		NAME = lab_log, 
		FILENAME = '/home/relict/desktop/db_lab/lablog.ldf', 
		SIZE = 5MB, 
		MAXSIZE = 20MB, 
		FILEGROWTH = 5%
	);
GO

USE lab;
GO

-- 2. Создание таблицы
CREATE TABLE Galaxy (
	GalaxyID INT PRIMARY KEY NOT NULL,
	NGCNumber Varchar(8) NOT NULL,
	Type INT NOT NULL,
	Size INT NOT NULL,
	Constellation INT NOT NULL,
	ApparentMagnitude Numeric(4, 2) NOT NULL
);
GO


-- 3. Добавление файловой группы и файла
ALTER DATABASE lab
ADD FILEGROUP newFG;
GO

ALTER DATABASE lab
ADD FILE (
    NAME = newFile,
    FILENAME = '/home/relict/desktop/db_lab/newFile.ndf',
    SIZE = 5MB,
    MAXSIZE = 10MB,
    FILEGROWTH = 5%
)
TO FILEGROUP newFG;
GO

-- 4. Файловая группа по умолчанию
ALTER DATABASE lab
MODIFY FILEGROUP newFG DEFAULT;
GO

-- 5. Создание второй таблицы
CREATE TABLE Star(
	StarID INT NOT NULL,
	StarCatalogNumber Varchar(8) NOT NULL,
	Size Numeric(7, 2) NOT NULL,
	Mass Numeric(7, 2) NULL,
	Constellation INT NOT NULL,
	Temperature INT NULL,
	Luminosity Numeric(7, 2) NOT NULL,
	SpectralClass INT NOT NULL,
	ApparentMagnitude Numeric(4, 2) NOT NULL,
	GalaxyID INT NULL,
	FOREIGN KEY (GalaxyID) REFERENCES Galaxy(GalaxyID),
	CONSTRAINT PK_Star PRIMARY KEY CLUSTERED (StarID)
);
GO

-- *. Перенос таблицы в другую файловую группу

ALTER TABLE Star
    DROP CONSTRAINT PK_Star;
GO

CREATE CLUSTERED INDEX IX_Star_StarID
    ON Star(StarID)
    ON [PRIMARY];
GO


-- 6. Удаление файловой группы

USE lab;
GO

ALTER DATABASE lab
MODIFY FILEGROUP [PRIMARY] DEFAULT;
GO

ALTER DATABASE lab
REMOVE FILE newFile;
GO

ALTER DATABASE lab
REMOVE FILEGROUP newFG;
GO


-- 7. Создание и удаление схемы
USE lab;
GO

CREATE SCHEMA newSchema;
GO

ALTER SCHEMA newSchema Transfer Star;
GO

ALTER SCHEMA dbo Transfer newSchema.Star;
GO

-- Либо вместо переноса таблицы можно её просто удалить
-- DROP TABLE newSchema.Star;

DROP SCHEMA newSchema;
GO

