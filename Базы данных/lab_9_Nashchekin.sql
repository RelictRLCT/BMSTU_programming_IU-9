-- Чтоб можно было прогнать скрипт несколько раз
USE master;
GO

IF DB_ID (N'lab9') IS NOT NULL
DROP DATABASE lab9;
GO

-- Создание базы
CREATE DATABASE lab9
	ON (
		NAME = lab9_dat, 
		FILENAME = '/home/relict/desktop/db_labs/db_lab_9/labdat.mdf', 
		SIZE = 10MB, 
		MAXSIZE = UNLIMITED, 
		FILEGROWTH = 10%
	)
	LOG ON (
		NAME = lab9_log, 
		FILENAME = '/home/relict/desktop/db_labs/db_lab_9/lablog.ldf', 
		SIZE = 5MB, 
		MAXSIZE = 20MB, 
		FILEGROWTH = 5%
	);
GO

USE lab9;
GO

CREATE TABLE Galaxy(
	GalaxyID INT PRIMARY KEY IDENTITY(1, 1) NOT NULL,
	NGCNumber Varchar(8) NOT NULL,
	Type INT NOT NULL,
	Size INT NOT NULL,
	Constellation INT NOT NULL,
	ApparentMagnitude Numeric(4, 2) NOT NULL,
);
GO

CREATE TABLE Satellite(
	SatelliteCatalogNumber Varchar(10) PRIMARY KEY NOT NULL,
	Size Numeric(7, 2) NOT NULL,
	Mass Numeric(7, 2) NULL,
	Period Numeric (7, 2) NULL,
	ApparentMagnitude Numeric(4, 2) NOT NULL,
);
GO

CREATE TABLE NaturalSat(
	SatelliteCatalogNumber Varchar(10) PRIMARY KEY NOT NULL,
	Temperature Numeric(7, 2) NULL,
	CompositionType INT NULL,
	FOREIGN KEY (SatelliteCatalogNumber) REFERENCES Satellite(SatelliteCatalogNumber) ON DELETE CASCADE,
);
GO

CREATE TABLE ArtificialSat(
	SatelliteCatalogNumber Varchar(10) PRIMARY KEY NOT NULL,
	Type INT NOT NULL,
	HeightAboveSurface Numeric(7, 2) NOT NULL,
	FOREIGN KEY (SatelliteCatalogNumber) REFERENCES Satellite(SatelliteCatalogNumber) ON DELETE CASCADE,
);
GO



---- 1. Триггеры для таблицы
-- INSERT
CREATE TRIGGER GalaxyInsertTrigger ON Galaxy
AFTER INSERT AS
SELECT NGCNumber, Constellation FROM inserted;
GO


INSERT INTO Galaxy(NGCNumber, Type, Size, Constellation, ApparentMagnitude)
	VALUES ('NGC1234', 6, 12, 3, 8);

INSERT INTO Galaxy(NGCNumber, Type, Size, Constellation, ApparentMagnitude)
	VALUES ('NGC1378', 3, 2, 38, 10);

INSERT INTO Galaxy(NGCNumber, Type, Size, Constellation, ApparentMagnitude)
	VALUES ('NGC138', 5, 8, 31, 16);

INSERT INTO Galaxy(NGCNumber, Type, Size, Constellation, ApparentMagnitude)
	VALUES ('NGC139', 6, 9, 32, 17), ('NGC140', 7, 10, 33, 18), ('NGC141', 8, 11, 34, 19);
GO

-- UPDATE
CREATE TRIGGER GalaxyUpdateTrigger ON Galaxy
AFTER UPDATE AS
IF UPDATE(Constellation)
	SELECT 'Объект с номером ' + d.NGCNumber + ' внезапно переехал из созвездия с номером ' + 
	CAST(d.Constellation AS Varchar) + ' в созвездие ' +
	CAST((SELECT i.Constellation FROM inserted AS i WHERE i.NGCNumber = d.NGCNumber) AS Varchar) AS Info
	FROM deleted as d
GO


UPDATE Galaxy
SET Constellation = 10
WHERE Size > 9
GO

-- DELETE
CREATE TRIGGER GalaxyDeleteTrigger ON Galaxy
AFTER DELETE AS
BEGIN
	IF EXISTS (SELECT * FROM deleted as d WHERE d.NGCNumber = 'NGC138')
		BEGIN
			RAISERROR('Объект NGC138 удалять нельзя ', 16, 1)
			ROLLBACK TRANSACTION;
		END;
	
	SELECT 'Удалён объект с номером ' + d.NGCNumber AS Info
	FROM deleted as d
END;
GO

DELETE FROM Galaxy
WHERE len(NGCNumber) > 6 
GO

--DELETE FROM Galaxy
--WHERE NGCNumber = 'NGC138'
--GO

SELECT * FROM Galaxy;
GO


---- 2. Триггер для представления 

CREATE VIEW SatelliteSubtypesView AS
SELECT
	s.SatelliteCatalogNumber, s.Size, s.Mass,
	s.Period, s.ApparentMagnitude, a.Type,
	a.HeightAboveSurface
FROM Satellite AS s
LEFT JOIN ArtificialSat AS a
ON s.SatelliteCatalogNumber = a.SatelliteCatalogNumber
GO



-- INSERT
CREATE TRIGGER SatelliteSubtypesViewInsertTrigger ON SatelliteSubtypesView
INSTEAD OF INSERT 
AS
BEGIN
	
	IF EXISTS (
        SELECT *
        FROM inserted
        WHERE Type IS NULL OR HeightAboveSurface IS NULL
    )
    
    BEGIN
        RAISERROR ('Некорректные значения: нужен подтип', 16, 1);
        RETURN;
    END;
	
	INSERT INTO Satellite(SatelliteCatalogNumber, Size, Mass, Period, ApparentMagnitude)
		(SELECT SatelliteCatalogNumber, Size, Mass, Period, ApparentMagnitude FROM inserted)
		
	INSERT INTO ArtificialSat(SatelliteCatalogNumber, Type, HeightAboveSurface)
		(SELECT SatelliteCatalogNumber, Type, HeightAboveSurface FROM inserted)

END;
GO


INSERT INTO SatelliteSubtypesView(SatelliteCatalogNumber, Size, Mass, Period, 
				ApparentMagnitude, Type, HeightAboveSurface)
VALUES ('OSP9437', 0.2, 0.1, 3, 12, 2, 2000);
GO

INSERT INTO SatelliteSubtypesView(SatelliteCatalogNumber, Size, Mass, Period, 
				ApparentMagnitude, Type, HeightAboveSurface)
VALUES ('OSP9440', 0.5, 0.3, 4, 11, 4, 2200);
GO

SELECT * FROM SatelliteSubtypesView
GO 


-- UPDATE
CREATE TRIGGER SatelliteSubtypesViewUpdateTrigger ON SatelliteSubtypesView
INSTEAD OF UPDATE
AS
BEGIN
	IF UPDATE(SatelliteCatalogNumber)
		RAISERROR('Запрещено менять номер в каталоге ', 16, 4)
		RETURN;
	
    UPDATE Satellite
    SET 
        Size = inserted.Size,
        Mass = inserted.Mass,
        Period = inserted.Period,
        ApparentMagnitude = inserted.ApparentMagnitude
    FROM Satellite
    INNER JOIN inserted ON Satellite.SatelliteCatalogNumber = inserted.SatelliteCatalogNumber;

    UPDATE ArtificialSat
    SET 
        Type = inserted.Type,
        HeightAboveSurface = inserted.HeightAboveSurface
    FROM ArtificialSat
    INNER JOIN inserted ON ArtificialSat.SatelliteCatalogNumber = inserted.SatelliteCatalogNumber;
END;
GO


UPDATE SatelliteSubtypesView 
SET Size = 123, Type = 3, SatelliteCatalogNumber = 'OSP9438'
WHERE SatelliteCatalogNumber = 'OSP9437'
GO

SELECT * FROM SatelliteSubtypesView 
GO



-- DELETE
CREATE TRIGGER SatelliteSubtypesViewDeleteTrigger ON SatelliteSubtypesView
INSTEAD OF DELETE
AS
BEGIN	
	DELETE FROM ArtificialSat
    WHERE SatelliteCatalogNumber IN (
        SELECT SatelliteCatalogNumber FROM deleted
    )

    DELETE FROM Satellite
    WHERE SatelliteCatalogNumber IN (
        SELECT SatelliteCatalogNumber FROM deleted
    )

END
GO


DELETE FROM SatelliteSubtypesView 
WHERE SatelliteCatalogNumber = 'OSP9437'
GO 

SELECT * FROM SatelliteSubtypesView 
GO


-- Бонус (Merge)

DROP TABLE IF EXISTS Galaxy;
DROP TABLE IF EXISTS NewGalaxy;
GO

CREATE TABLE Galaxy(
	GalaxyID INT PRIMARY KEY IDENTITY(1, 1) NOT NULL,
	NGCNumber Varchar(8) NOT NULL,
	Type INT NOT NULL,
	Size INT NOT NULL,
	Constellation INT NOT NULL,
	ApparentMagnitude Numeric(4, 2) NOT NULL,
);
GO

CREATE TABLE NewGalaxy(
	GalaxyID INT PRIMARY KEY IDENTITY(1, 1) NOT NULL,
	NGCNumber Varchar(8) NOT NULL,
	Type INT NOT NULL,
	Size INT NOT NULL,
	Constellation INT NOT NULL,
	ApparentMagnitude Numeric(4, 2) NOT NULL,
);
GO



INSERT INTO Galaxy(NGCNumber, Type, Size, Constellation, ApparentMagnitude)
	VALUES 
	('NGC1234', 6, 12, 3, 8),
	('NGC1378', 3, 2, 38, 10),
	('NGC138', 5, 8, 31, 16),
	('NGC139', 6, 9, 32, 17),
	('NGC140', 7, 10, 33, 18),
	('NGC141', 8, 11, 34, 19);

INSERT INTO NewGalaxy(NGCNumber, Type, Size, Constellation, ApparentMagnitude)
	VALUES 
	('NGC1234', 6, 13, 3, 8.5),
	('NGC1378', 3, 2, 38, 10),
	('NGC138', 5, 8, 31, 16),
	('NGC139', 6, 9, 32, 17),
	('NGC140', 7, 10, 33, 18),
	('NGC141', 8, 11, 34, 19),
	('NGC142', 10, 5, 12, 10);

SELECT * FROM Galaxy;
GO

CREATE TRIGGER GalaxyInsertTriggerMERGE
ON Galaxy
AFTER INSERT
AS 
BEGIN 
	SELECT 'Вставлено!', * FROM inserted;
END;
GO

CREATE TRIGGER GalaxyUpdateTriggerMERGE
ON Galaxy
AFTER UPDATE
AS 
BEGIN 
	SELECT 'Обновлено!', * FROM inserted;
END;
GO

CREATE TRIGGER GalaxyDeleteTriggerMERGE
ON Galaxy
AFTER DELETE
AS 
BEGIN 
	SELECT 'Удалено!', * FROM deleted;
END;
GO

MERGE INTO Galaxy AS t
USING NewGalaxy AS s
ON t.NGCNumber = s.NGCNumber
WHEN MATCHED THEN
    UPDATE SET 
        t.Type = s.Type,
        t.Size = s.Size,
        t.Constellation = s.Constellation,
        t.ApparentMagnitude = s.ApparentMagnitude
WHEN NOT MATCHED BY TARGET THEN
    INSERT (NGCNumber, Type, Size, Constellation, ApparentMagnitude)
    VALUES (s.NGCNumber, s.Type, s.Size, s.Constellation, s.ApparentMagnitude)
WHEN NOT MATCHED BY SOURCE THEN
    DELETE;

SELECT * FROM Galaxy;
GO
