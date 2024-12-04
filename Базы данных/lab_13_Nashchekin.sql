-- Чтоб можно было прогнать скрипт несколько раз
USE master;
GO

IF DB_ID (N'lab13_1') IS NOT NULL
DROP DATABASE lab13_1;
GO

IF DB_ID (N'lab13_2') IS NOT NULL
    DROP DATABASE lab13_2;
GO

-- Создание баз
CREATE DATABASE lab13_1
	ON (
		NAME = lab13_1_dat,
		FILENAME = '/home/relict/desktop/db_labs/db_lab_13/labdat_1.mdf',
		SIZE = 10MB, 
		MAXSIZE = UNLIMITED, 
		FILEGROWTH = 10%
	)
	LOG ON (
		NAME = lab13_1_log,
		FILENAME = '/home/relict/desktop/db_labs/db_lab_13/lablog_1.ldf',
		SIZE = 5MB, 
		MAXSIZE = 20MB, 
		FILEGROWTH = 5%
	);
GO

CREATE DATABASE lab13_2
    ON (
    NAME = lab13_2_dat,
    FILENAME = '/home/relict/desktop/db_labs/db_lab_13/labdat_2.mdf',
    SIZE = 10MB,
    MAXSIZE = UNLIMITED,
    FILEGROWTH = 10%
    )
    LOG ON (
    NAME = lab13_2_log,
    FILENAME = '/home/relict/desktop/db_labs/db_lab_13/lablog_2.ldf',
    SIZE = 5MB,
    MAXSIZE = 20MB,
    FILEGROWTH = 5%
    );
GO

USE lab13_1;
GO

CREATE TABLE Galaxy(
       GalaxyID INT PRIMARY KEY NOT NULL CHECK (GalaxyID < 5),
       NGCNumber Varchar(8) UNIQUE NOT NULL,
       Type INT NOT NULL,
       Size INT NOT NULL,
       Constellation INT NOT NULL,
       ApparentMagnitude Numeric(4, 2) NOT NULL,

-- в самой полной классификации 18 типов
       CONSTRAINT CHK_Type
           CHECK ([Type] >= 1 AND [Type] <= 18),

-- 88 созвездий
       CONSTRAINT CHK_Constellation_Galaxy
           CHECK ([Constellation] >= 1 AND [Constellation] <= 88)

);
GO


USE lab13_2;
GO

CREATE TABLE Galaxy(
       GalaxyID INT PRIMARY KEY NOT NULL CHECK (GalaxyID >= 5),
       NGCNumber Varchar(8) UNIQUE NOT NULL,
       Type INT NOT NULL,
       Size INT NOT NULL,
       Constellation INT NOT NULL,
       ApparentMagnitude Numeric(4, 2) NOT NULL,

-- в самой полной классификации 18 типов
       CONSTRAINT CHK_Type
           CHECK ([Type] >= 1 AND [Type] <= 18),

-- 88 созвездий
       CONSTRAINT CHK_Constellation_Galaxy
           CHECK ([Constellation] >= 1 AND [Constellation] <= 88)

);
GO

USE lab13_1;
GO

CREATE VIEW GalaxyView
AS
    SELECT * FROM lab13_1.dbo.Galaxy
    UNION ALL
    SELECT * FROM lab13_2.dbo.Galaxy
GO

-- На моём выпуске SQL Server недоступен 'lazy schema validation'
-- EXEC sp_serveroption 'OtherServer',
--      'lazy schema validation', 'true'

INSERT INTO GalaxyView(GalaxyID, NGCNumber, Type, Size, Constellation, ApparentMagnitude)
	VALUES 
		( 1, 'NGC13', 4, 11, 22, 11),
        (4, 'NGC25', 2, 11, 22, 15),
		(7, 'NGC22', 5, 12, 23, 12),
        (24, 'NGC28', 3, 100, 2, 14),
        (5, 'NGC29', 4, 115, 12, 10)
GO;


SELECT * FROM lab13_1.dbo.Galaxy
SELECT * FROM lab13_2.dbo.Galaxy
SELECT * FROM GalaxyView
GO


UPDATE GalaxyView
    SET Type = 8 WHERE GalaxyID = 7
GO

SELECT * FROM lab13_1.dbo.Galaxy
SELECT * FROM lab13_2.dbo.Galaxy
SELECT * FROM GalaxyView
GO


DELETE FROM GalaxyView
    WHERE ApparentMagnitude > 13
GO

SELECT * FROM lab13_1.dbo.Galaxy
SELECT * FROM lab13_2.dbo.Galaxy
SELECT * FROM GalaxyView
GO
