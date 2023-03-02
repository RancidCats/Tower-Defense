using UnityEngine;

public class BuildManager : MonoBehaviour
{
    public static BuildManager instance;
    private void Awake()
    {
        instance = this;
    }

    private BuyTurret _turretToBuild;
    public GameObject barrack;
    public GameObject archer;
    public GameObject wizard;
    public GameObject cannon;
    public bool build { get { return _turretToBuild != null; } }
    public bool money { get { return Stats.money >= _turretToBuild.cost; } }
    public void SelectTurret(BuyTurret turret)
    {
        _turretToBuild = turret;
    }
    public void BuildTurret(Platforms platform)
    {
        if (Stats.money < _turretToBuild.cost)
        {
            return;
        }
        Stats.money -= _turretToBuild.cost;
        GameObject turret = (GameObject)Instantiate(_turretToBuild.turret, platform.transform.position, Quaternion.identity);
        platform.turret = turret;
    }
}
