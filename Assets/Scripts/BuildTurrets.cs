using UnityEngine;

public class BuildTurrets : MonoBehaviour
{
    BuildManager buildManager;
    public BuyTurret barrack;
    public BuyTurret archer;
    public BuyTurret cannon;
    public BuyTurret wizard;

    private void Start()
    {
        buildManager = BuildManager.instance;
    }
    public void BuildBarrack()
    {
        buildManager.SelectTurret(barrack);
    }
    public void BuildTowerArcher()
    {
        buildManager.SelectTurret(archer);
    }
    public void BuildCannon()
    {
        buildManager.SelectTurret(cannon);

    }
    public void BuildTowerWizard()
    {
        buildManager.SelectTurret(wizard);
    }
}
