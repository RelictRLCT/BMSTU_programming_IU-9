USE lab13_1;
GO

--DROP TABLE Star;
CREATE TABLE Star(
     StarID INT PRIMARY KEY NOT NULL,
     StarCatalogNumber Varchar(8) UNIQUE NOT NULL,
     Size Numeric(7, 2) NOT NULL,
     Mass Numeric(7, 2) NULL,
     Constellation INT NOT NULL,

-- 88 созвездий
     CONSTRAINT CHK_Constellation_Star
         CHECK ([Constellation] >= 1 AND [Constellation] <= 88)
);
GO

USE lab13_2;
GO

--DROP TABLE Star;
CREATE TABLE Star(
     StarID INT PRIMARY KEY NOT NULL,
     Temperature INT NULL,
     Luminosity Numeric(7, 2) NOT NULL,
     SpectralClass INT NOT NULL,
     ApparentMagnitude Numeric(4, 2) NOT NULL,

-- 7 спектральных классов
     CONSTRAINT CHK_SpecrtalClass
         CHECK ([SpectralClass] >= 1 AND [SpectralClass] <= 7)
);
GO

USE lab13_1;
GO

--DROP VIEW StarView
CREATE VIEW StarView
AS
    SELECT s1.StarID, s1.StarCatalogNumber, s1.Size, s1.Mass,
           s1.Constellation, s2.Temperature, s2.Luminosity, s2.SpectralClass,
           s2.ApparentMagnitude
        FROM lab13_1.dbo.Star s1, lab13_2.dbo.Star s2
        WHERE s1.StarID = s2.StarID
GO

--DROP TRIGGER StarViewInsertTrigger
-- INSERT
CREATE TRIGGER StarViewInsertTrigger ON StarView
    INSTEAD OF INSERT
    AS
BEGIN
    INSERT INTO lab13_1.dbo.Star(StarID, StarCatalogNumber, Size, Mass, Constellation)
        SELECT i.StarID, i.StarCatalogNumber, i.Size, i.Mass, i.Constellation FROM inserted AS i

    INSERT INTO lab13_2.dbo.Star(StarID, Temperature, Luminosity, SpectralClass, ApparentMagnitude)
        SELECT i.StarID, i.Temperature, i.Luminosity, i.SpectralClass, i.ApparentMagnitude FROM inserted AS i
END
GO

INSERT INTO StarView(StarID, StarCatalogNumber, Size, Mass, Constellation,
                     Temperature, Luminosity, SpectralClass, ApparentMagnitude)
	VALUES 
		(1, 'NGC134', 4, 11, 1, 11000, 1, 2, 12),
        (2, 'NGC256', 6, 12, 2, 15000, 2, 3, 11),
		(3, 'NGC222', 0.2, 1, 21, 12000, 1.5, 4, 5),
        (4, 'NGC289', 100, 100, 21, 14000, 1, 1, 7),
        (5, 'NGC299', 40, 115, 12, 10000, 3, 2, 6)
GO;

SELECT * FROM lab13_1.dbo.Star
SELECT * FROM lab13_2.dbo.Star
SELECT * FROM StarView
GO

-- UPDATE
CREATE TRIGGER StarViewUpdateTrigger ON StarView
    INSTEAD OF UPDATE
    AS
BEGIN
    IF UPDATE(StarID)
    BEGIN
        RAISERROR(N'Запрещено менять StarID ', 16, 1);
        RETURN;
    END

    UPDATE lab13_1.dbo.Star
    SET StarID = i.StarID, StarCatalogNumber = i.StarCatalogNumber,
        Size = i.Size, Mass = i.Mass, Constellation = i.Constellation
    FROM lab13_1.dbo.Star AS s1
        INNER JOIN inserted AS i
            ON s1.StarID = i.StarID

    UPDATE lab13_2.dbo.Star
    SET StarID = i.StarID, Temperature = i.Temperature,
        Luminosity = i.Luminosity, SpectralClass = i.SpectralClass,
        ApparentMagnitude = i.ApparentMagnitude
    FROM lab13_2.dbo.Star AS s2
             INNER JOIN inserted AS i
                        ON s2.StarID = i.StarID
END
GO

UPDATE StarView
    SET Mass = 12, Temperature = 12002
    WHERE StarCatalogNumber = 'NGC134'

SELECT * FROM lab13_1.dbo.Star
SELECT * FROM lab13_2.dbo.Star
SELECT * FROM StarView
GO

-- DELETE
CREATE TRIGGER StarViewDeleteTrigger ON StarView
    INSTEAD OF DELETE
    AS
BEGIN

    DELETE FROM lab13_1.dbo.Star
    WHERE StarID IN (
        SELECT StarID FROM deleted
    )

    DELETE FROM lab13_2.dbo.Star
    WHERE StarID IN (
        SELECT StarID FROM deleted
    )

END
GO

DELETE FROM StarView
    WHERE SpectralClass > 3
GO

SELECT * FROM lab13_1.dbo.Star
SELECT * FROM lab13_2.dbo.Star
SELECT * FROM StarView
GO
